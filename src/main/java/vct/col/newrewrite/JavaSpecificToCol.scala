package vct.col.newrewrite

import hre.util.{FuncTools, ScopedStack}
import vct.col.ast.Constant._
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.util.SuccessionMap
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, Java, JavaTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefJavaClass, RefJavaField, RefJavaLocalDeclaration, RefJavaMethod, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefUnloadedJavaNamespace, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}

import scala.collection.mutable

case class JavaSpecificToCol() extends Rewriter {
  val namespace: ScopedStack[JavaNamespace] = ScopedStack()
  val javaInstanceClassSuccessor: SuccessionMap[JavaClassOrInterface, Class] = SuccessionMap()
  val javaStaticsClassSuccessor: SuccessionMap[JavaClassOrInterface, Class] = SuccessionMap()
  val javaStaticsFunctionSuccessor: SuccessionMap[JavaClassOrInterface, Function] = SuccessionMap()

  val javaFieldsSuccessor: SuccessionMap[(JavaFields, Int), InstanceField] = SuccessionMap()
  val javaLocalsSuccessor: SuccessionMap[(JavaLocalDeclaration, Int), Variable] = SuccessionMap()

  val javaDefaultConstructor: mutable.Map[JavaClassOrInterface, JavaConstructor] = mutable.Map()

  val currentThis: ScopedStack[Expr] = ScopedStack()
  val currentJavaClass: ScopedStack[JavaClass] = ScopedStack()

  def isJavaStatic(decl: ClassDeclaration): Boolean = decl match {
    case JavaSharedInitialization(isStatic, _) => isStatic
    case fields: JavaFields => fields.modifiers.contains(JavaStatic()(DiagnosticOrigin))
    case method: JavaMethod => method.modifiers.contains(JavaStatic()(DiagnosticOrigin))
    case _: JavaConstructor => false
    case _: ClassDeclaration => false // FIXME we should have a way of translating static specification-type declarations
  }

  def makeJavaClass(decls: Seq[ClassDeclaration], ref: Ref[Class])(implicit o: Origin): Unit = {
    // First, declare all the fields, so we can refer to them.
    decls.foreach {
      case fields @ JavaFields(mods, t, decls) =>
        for(((_, dims, _), idx) <- decls.zipWithIndex) {
          javaFieldsSuccessor((fields, idx)) =
            new InstanceField(
              t = FuncTools.repeat(TArray(_), dims, t),
              flags = mods.collect { case JavaFinal() => new Final() }.toSet)
        }
      case _ =>
    }

    // Each constructor performs in order:
    // 1. the inline initialization of all fields

    val fieldInit = (diz: Expr) => Block(decls.collect {
      case fields @ JavaFields(_, _, decls) =>
        Block(for(((_, _, init), idx) <- decls.zipWithIndex if init.nonEmpty)
          yield Assign(Deref(diz, javaFieldsSuccessor.ref((fields, idx))), dispatch(init.get))
        )
    })

    // 2. the shared initialization blocks

    val sharedInit = (diz: Expr) => {
      currentThis.having(diz) {
        Block(decls.collect {
          case JavaSharedInitialization(_, init) => dispatch(init)
        })
      }
    }

    // 3. the body of the constructor

    val declsDefault = if(decls.collect { case _: JavaConstructor => () }.isEmpty) {
      javaDefaultConstructor(currentJavaClass.head) = JavaConstructor(
        modifiers = Nil,
        name = "",
        parameters = Nil,
        typeParameters = Nil,
        signals = Nil,
        body = Block(Nil),
        contract = ApplicableContract(
          requires = true,
          ensures = Star.fold(decls.collect {
            case fields @ JavaFields(_, _, decls) =>
              decls.indices.map(decl => {
                Perm(Deref(currentThis.head, javaFieldsSuccessor.ref((fields, decl))), WritePerm())
              })
          }.flatten),
          contextEverywhere = true, signals = Nil, givenArgs = Nil, yieldsArgs = Nil
        )
      )
      javaDefaultConstructor(currentJavaClass.head) +: decls
    } else decls

    declsDefault.foreach {
      case cons @ JavaConstructor(mods, _, params, typeParams, signals, body, contract) =>
        implicit val o: Origin = cons.o
        val t = TClass(ref)
        val resVar = new Variable(t)
        val res = Local(resVar.ref)
        currentThis.having(res) {
          new Procedure(
            returnType = t,
            args = collectInScope(variableScopes) {
              params.foreach(dispatch)
            },
            outArgs = Nil,
            body = Some(Scope(Seq(resVar), Block(Seq(
              Assign(res, NewObject(ref)),
              fieldInit(res),
              sharedInit(res),
              dispatch(body),
            )))),
            contract = dispatch(contract),
          )(null).succeedDefault(this, cons)
        }
      case method @ JavaMethod(mods, returnType, dims, _, params, typeParams, signals, body, contract) =>
        new InstanceMethod(
          returnType = dispatch(returnType),
          args = collectInScope(variableScopes) { params.foreach(dispatch) },
          outArgs = Nil,
          body = body.map(dispatch),
          contract = dispatch(contract),
        )(null).succeedDefault(this, method)
      case _: JavaSharedInitialization =>
      case _: JavaFields =>
      case other => dispatch(other)
    }
  }

  override def dispatch(decl: Declaration): Unit = decl match {
    case ns: JavaNamespace =>
      namespace.having(ns) {
        // Do not enter a scope, so classes of the namespace are declared to the program.
        ns.declarations.foreach(dispatch)
      }
    case cls @ JavaClass(_, mods, typeParams, ext, imp, decls) =>
      implicit val o: Origin = cls.o

      currentJavaClass.having(cls) {
        val supports = (dispatch(cls.ext) +: cls.imp.map(dispatch)).map {
          case TClass(cls) => cls
          case _ => ???
        }

        val instanceClass = new Class(collectInScope(classScopes) {
          val diz = AmbiguousThis()
          diz.ref = Some(TClass(javaInstanceClassSuccessor.ref(cls)))
          currentThis.having(diz) {
            makeJavaClass(decls.filter(!isJavaStatic(_)), javaInstanceClassSuccessor.ref(cls))
          }
        }, supports)

        if(instanceClass.declarations.nonEmpty) {
          instanceClass.declareDefault(this)
          javaInstanceClassSuccessor(cls) = instanceClass
        }

        val staticsClass = new Class(collectInScope(classScopes) {
          val diz = AmbiguousThis()
          diz.ref = Some(TClass(javaStaticsClassSuccessor.ref(cls)))
          currentThis.having(diz) {
            makeJavaClass(decls.filter(isJavaStatic), javaStaticsClassSuccessor.ref(cls))
          }
        }, Nil)

        if(staticsClass.declarations.nonEmpty) {
          staticsClass.declareDefault(this)
          val singleton = new Function(TClass(staticsClass.ref), Nil, None, ApplicableContract(true, true, true, Nil, Nil, Nil))(null)
          singleton.declareDefault(this)
          javaStaticsClassSuccessor(cls) = staticsClass
          javaStaticsFunctionSuccessor(cls) = singleton
        }
      }
    case cls @ JavaInterface(_, mods, typeParams, ext, decls) =>
      ???
    case cls: Class =>
      val diz = AmbiguousThis()(cls.o)
      diz.ref = Some(TClass(typedSucc[Class](cls)))
      currentThis.having(diz) {
        rewriteDefault(cls)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement): Statement = stat match {
    case scope @ Scope(locals, body) =>
      def scanScope(node: Node): Unit = node match {
        case Scope(_, _) =>
        case JavaLocalDeclarationStatement(locals @ JavaLocalDeclaration(mods, t, decls)) =>
          implicit val o: Origin = node.o
          decls.zipWithIndex.foreach {
            case ((_, dims, _), idx) =>
              val v = new Variable(FuncTools.repeat(TArray(_), dims, dispatch(t)))
              javaLocalsSuccessor((locals, idx)) = v
              v.declareDefault(this)
          }
        case other => other.subnodes.foreach(scanScope)
      }

      scope.rewrite(locals = collectInScope(variableScopes) {
        locals.foreach(dispatch)
        scanScope(body)
      })

    case JavaLocalDeclarationStatement(locals @ JavaLocalDeclaration(_, _, decls)) =>
      implicit val o: Origin = locals.o
      Block(for(((_, _, init), i) <- decls.zipWithIndex if init.nonEmpty)
        yield Assign(Local(javaLocalsSuccessor.ref((locals, i))), dispatch(init.get))
      )

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = e match {
    case AmbiguousThis() => currentThis.head

    case local @ JavaLocal(_) =>
      implicit val o: Origin = local.o

      local.ref.get match {
        case RefAxiomaticDataType(decl) => ???
        case RefVariable(decl) => Local(typedSucc[Variable](decl))
        case RefUnloadedJavaNamespace(names) => ???
        case RefJavaClass(decl) => ???
        case RefJavaField(decls, idx) =>
          if(decls.modifiers.contains(JavaStatic())) {
            Deref(
              obj = FunctionInvocation(javaStaticsFunctionSuccessor.ref(currentJavaClass.head), Nil)(null),
              ref = javaFieldsSuccessor.ref((decls, idx)),
            )
          } else {
            Deref(currentThis.head, javaFieldsSuccessor.ref((decls, idx)))
          }
        case RefJavaLocalDeclaration(decls, idx) =>
          Local(javaLocalsSuccessor.ref((decls, idx)))
      }

    case deref @ JavaDeref(obj, _) =>
      implicit val o: Origin = deref.o
      deref.ref.get match {
        case RefAxiomaticDataType(decl) => ???
        case RefModel(decl) => ???
        case RefJavaClass(decl) => ???
        case RefModelField(decl) => ModelDeref(dispatch(obj), typedSucc[ModelField](decl))
        case RefUnloadedJavaNamespace(names) => ???
        case RefJavaField(decls, idx) =>
          Deref(dispatch(obj), javaFieldsSuccessor.ref((decls, idx)))
        case BuiltinField(f) => f(dispatch(obj))
      }

    case JavaLiteralArray(_) => ???

    case inv @ JavaInvocation(obj, typeParams, _, args, _, _) =>
      implicit val o: Origin = inv.o
      inv.ref.get match {
        case RefFunction(decl) =>
          FunctionInvocation(typedSucc[Function](decl), args.map(dispatch))(null)
        case RefProcedure(decl) =>
          ProcedureInvocation(typedSucc[Procedure](decl), args.map(dispatch), Nil)(null)
        case RefPredicate(decl) =>
          PredicateApply(typedSucc[Predicate](decl), args.map(dispatch))
        case RefInstanceFunction(decl) =>
          InstanceFunctionInvocation(obj.map(dispatch).getOrElse(currentThis.head), typedSucc[InstanceFunction](decl), args.map(dispatch))(null)
        case RefInstanceMethod(decl) =>
          MethodInvocation(obj.map(dispatch).getOrElse(currentThis.head), typedSucc[InstanceMethod](decl), args.map(dispatch), Nil)(null)
        case RefInstancePredicate(decl) =>
          InstancePredicateApply(obj.map(dispatch).getOrElse(currentThis.head), typedSucc[InstancePredicate](decl), args.map(dispatch))
        case RefADTFunction(decl) =>
          ADTFunctionInvocation(typedSucc[ADTFunction](decl), args.map(dispatch))
        case RefModelProcess(decl) =>
          ???
        case RefModelAction(decl) =>
          ???
        case RefJavaMethod(decl) =>
          if(decl.modifiers.contains(JavaStatic())) {
            MethodInvocation(
              obj = FunctionInvocation(javaStaticsFunctionSuccessor.ref(currentJavaClass.head), Nil)(null),
              ref = typedSucc[InstanceMethod](decl),
              args = args.map(dispatch), outArgs = Nil)(null)
          } else {
            MethodInvocation(
              obj = obj.map(dispatch).getOrElse(currentThis.head),
              ref = typedSucc[InstanceMethod](decl),
              args = args.map(dispatch), outArgs = Nil)(null)
          }
        case BuiltinInstanceMethod(f) =>
          f(dispatch(obj.get))(args.map(dispatch))
      }

    case inv @ JavaNewClass(args, typeParams, t @ JavaTClass(_)) =>
      implicit val o: Origin = inv.o
      t.ref.get match {
        case RefAxiomaticDataType(decl) => ???
        case RefModel(decl) => ???
        case RefJavaClass(decl) =>
          val cons = decl.decls.collect {
            case cons: JavaConstructor if cons.parameters.size == args.size => cons
          }.find(_.parameters.zip(args).forall { case (arg, v) => arg.t.superTypeOf(v.t) })
            .getOrElse(javaDefaultConstructor(decl))

          // TODO reference to default constructor is not lazy!

          ProcedureInvocation(typedSucc[Procedure](cons), args.map(dispatch), Nil)(null)
      }

    case JavaNewLiteralArray(_, _, _) => ???

    case JavaNewDefaultArray(_, _, _) => ???

    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type): Type = t match {
    case JavaTUnion(names) =>
      if(names.size == 1) {
        dispatch(names.head)
      } else {
        ???
      }
    case tClass @ JavaTClass(_) =>
      tClass.ref.get match {
        case RefAxiomaticDataType(decl) => TAxiomatic(typedSucc[AxiomaticDataType](decl), Nil)
        case RefModel(decl) => TModel(typedSucc[Model](decl))
        case RefJavaClass(decl) => TClass(javaInstanceClassSuccessor.ref(decl))
      }
    case other => rewriteDefault(other)
  }
}
