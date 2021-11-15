package vct.col.newrewrite.lang

import hre.util.{FuncTools, ScopedStack}
import vct.col.ast.Constant._
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.resolve._
import vct.col.rewrite.Rewriter
import vct.result.VerificationResult.{Unreachable, UserError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

case class LangSpecificToCol() extends Rewriter {
  case class NotAValue(value: Expr) extends UserError {
    override def code: String = "notAValue"
    override def text: String = value.o.messageInContext("Could not resolve this expression to a value.")
  }

  val namespace: ScopedStack[JavaNamespace] = ScopedStack()
  val javaInstanceClassSuccessor: SuccessionMap[JavaClassOrInterface, Class] = SuccessionMap()
  val javaStaticsClassSuccessor: SuccessionMap[JavaClassOrInterface, Class] = SuccessionMap()
  val javaStaticsFunctionSuccessor: SuccessionMap[JavaClassOrInterface, Function] = SuccessionMap()

  val javaFieldsSuccessor: SuccessionMap[(JavaFields, Int), InstanceField] = SuccessionMap()
  val javaLocalsSuccessor: SuccessionMap[(JavaLocalDeclaration, Int), Variable] = SuccessionMap()

  val javaDefaultConstructor: mutable.Map[JavaClassOrInterface, JavaConstructor] = mutable.Map()
  val pvlDefaultConstructor: SuccessionMap[Class, Procedure] = SuccessionMap()

  val currentThis: ScopedStack[Expr] = ScopedStack()
  val currentJavaClass: ScopedStack[JavaClassOrInterface] = ScopedStack()

  case class JavaInlineArrayInitializerOrigin(inner: Origin) extends Origin {
    override def preferredName: String = "arrayInitializer"
    override def messageInContext(message: String): String = inner.messageInContext(message)
  }

  case class InvalidArrayInitializerNesting(initializer: JavaLiteralArray) extends UserError {
    override def text: String = initializer.o.messageInContext("This literal array is nested more deeply than its indicated type allows.")
    override def code: String = "invalidNesting"
  }

  def isJavaStatic(decl: ClassDeclaration): Boolean = decl match {
    case init: JavaSharedInitialization => init.isStatic
    case fields: JavaFields => fields.modifiers.contains(JavaStatic()(DiagnosticOrigin))
    case method: JavaMethod => method.modifiers.contains(JavaStatic()(DiagnosticOrigin))
    case _: JavaConstructor => false
    case _: ClassDeclaration => false // FIXME we should have a way of translating static specification-type declarations
  }

  def makeJavaClass(decls: Seq[ClassDeclaration], ref: Ref[Class])(implicit o: Origin): Unit = {
    // First, declare all the fields, so we can refer to them.
    decls.foreach {
      case fields: JavaFields =>
        for(((_, dims, _), idx) <- fields.decls.zipWithIndex) {
          javaFieldsSuccessor((fields, idx)) =
            new InstanceField(
              t = FuncTools.repeat(TArray(_), dims, fields.t),
              flags = fields.modifiers.collect { case JavaFinal() => new Final() }.toSet)
        }
      case _ =>
    }

    // Each constructor performs in order:
    // 1. the inline initialization of all fields

    val fieldInit = (diz: Expr) => Block(decls.collect {
      case fields: JavaFields =>
        Block(for(((_, _, init), idx) <- fields.decls.zipWithIndex if init.nonEmpty)
          yield assignField(diz, javaFieldsSuccessor.ref((fields, idx)), dispatch(init.get))
        )
    })

    // 2. the shared initialization blocks

    val sharedInit = (diz: Expr) => {
      currentThis.having(diz) {
        Block(decls.collect {
          case init: JavaSharedInitialization => dispatch(init.initialization)
        })
      }
    }

    // 3. the body of the constructor

    val declsDefault = if(decls.collect { case _: JavaConstructor => () }.isEmpty) {
      javaDefaultConstructor(currentJavaClass.head) = new JavaConstructor(
        modifiers = Nil,
        name = "",
        parameters = Nil,
        typeParameters = Nil,
        signals = Nil,
        body = Block(Nil),
        contract = ApplicableContract(
          requires = true,
          ensures = Star.fold(decls.collect {
            case fields: JavaFields =>
              fields.decls.indices.map(decl =>
                fieldPerm(currentThis.head, javaFieldsSuccessor.ref((fields, decl)), WritePerm()))
          }.flatten),
          contextEverywhere = true, signals = Nil, givenArgs = Nil, yieldsArgs = Nil
        )
      )
      javaDefaultConstructor(currentJavaClass.head) +: decls
    } else decls

    declsDefault.foreach {
      case cons: JavaConstructor =>
        implicit val o: Origin = cons.o
        val t = TClass(ref)
        val resVar = new Variable(t)
        val res = Local(resVar.ref)
        currentThis.having(res) {
          new Procedure(
            returnType = t,
            args = collectInScope(variableScopes) {
              cons.parameters.foreach(dispatch)
            },
            outArgs = Nil, typeArgs = Nil,
            body = Some(Scope(Seq(resVar), Block(Seq(
              Assign(res, NewObject(ref)),
              fieldInit(res),
              sharedInit(res),
              dispatch(cons.body),
              Return(res),
            )))),
            contract = dispatch(cons.contract),
          )(null).succeedDefault(this, cons)
        }
      case method: JavaMethod =>
        new InstanceMethod(
          returnType = dispatch(method.returnType),
          args = collectInScope(variableScopes) { method.parameters.foreach(dispatch) },
          outArgs = Nil, typeArgs = Nil,
          body = method.body.map(dispatch),
          contract = dispatch(method.contract),
        )(null).succeedDefault(this, method)
      case _: JavaSharedInitialization =>
      case _: JavaFields =>
      case other => dispatch(other)
    }
  }

  override def dispatch(decl: Declaration): Unit = decl match {
    case model: Model =>
      implicit val o: Origin = model.o
      val diz = AmbiguousThis()
      diz.ref = Some(TModel(model.ref))
      currentThis.having(diz) { model.rewrite().succeedDefault(this, model) }

    case ns: JavaNamespace =>
      namespace.having(ns) {
        // Do not enter a scope, so classes of the namespace are declared to the program.
        ns.declarations.foreach(dispatch)
      }

    case cls: JavaClassOrInterface =>
      implicit val o: Origin = cls.o

      currentJavaClass.having(cls) {
        val supports = cls.supports.map(dispatch).flatMap {
          case TClass(ref) => Seq(ref)
          case TAny() => Nil
          case _ => ???
        }

        val instDecls = cls.decls.filter(!isJavaStatic(_))
        val staticDecls = cls.decls.filter(isJavaStatic)

        if(instDecls.nonEmpty) {
          val instanceClass = new Class(collectInScope(classScopes) {
            val diz = AmbiguousThis()
            diz.ref = Some(TClass(javaInstanceClassSuccessor.ref(cls)))
            currentThis.having(diz) {
              makeJavaClass(instDecls, javaInstanceClassSuccessor.ref(cls))
            }
          }, supports)

          instanceClass.declareDefault(this)
          javaInstanceClassSuccessor(cls) = instanceClass
        }

        if(staticDecls.nonEmpty) {
          val staticsClass = new Class(collectInScope(classScopes) {
            val diz = AmbiguousThis()
            diz.ref = Some(TClass(javaStaticsClassSuccessor.ref(cls)))
            currentThis.having(diz) {
              makeJavaClass(staticDecls, javaStaticsClassSuccessor.ref(cls))
            }
          }, Nil)

          staticsClass.declareDefault(this)
          val singleton = new Function(TClass(staticsClass.ref), Nil, Nil, None, ApplicableContract(true, true, true, Nil, Nil, Nil))(null)
          singleton.declareDefault(this)
          javaStaticsClassSuccessor(cls) = staticsClass
          javaStaticsFunctionSuccessor(cls) = singleton
        }
      }

    case cls: Class =>
      val diz = AmbiguousThis()(cls.o)
      diz.ref = Some(TClass(succ[Class](cls)))

      val decls = currentThis.having(diz) {
        collectInScope(classScopes) {
          cls.declarations.foreach(dispatch)

          if(cls.declarations.collectFirst { case _: PVLConstructor => () }.isEmpty) {
            implicit val o: Origin = cls.o
            val t = TClass(succ[Class](cls))
            val resVar = new Variable(t)
            val res = Local(resVar.ref)

            pvlDefaultConstructor(cls) = new Procedure(
              TClass(succ[Class](cls)),
              Nil, Nil, Nil,
              Some(Scope(Seq(resVar), Block(Seq(
                Assign(res, NewObject(succ[Class](cls))),
                Return(res),
              )))),
              ApplicableContract(
                true,
                Star.fold(cls.declarations.collect {
                  case field: InstanceField =>
                    fieldPerm(currentThis.head, succ[InstanceField](field), WritePerm())
                }), true, Nil, Nil, Nil,
              )
            )(null)

            pvlDefaultConstructor(cls).declareDefault(this)
          }
        }
      }

      cls.rewrite(decls).succeedDefault(this, cls)

    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement): Statement = stat match {
    case scope @ Scope(locals, body) =>
      def scanScope(node: Node): Unit = node match {
        case Scope(_, _) =>
        case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration) =>
          implicit val o: Origin = node.o
          locals.decls.zipWithIndex.foreach {
            case ((_, dims, _), idx) =>
              val v = new Variable(FuncTools.repeat(TArray(_), dims, dispatch(locals.t)))
              javaLocalsSuccessor((locals, idx)) = v
              v.declareDefault(this)
          }
        case other => other.subnodes.foreach(scanScope)
      }

      scope.rewrite(locals = collectInScope(variableScopes) {
        locals.foreach(dispatch)
        scanScope(body)
      })

    case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration) =>
      implicit val o: Origin = locals.o
      Block(for(((_, _, init), i) <- locals.decls.zipWithIndex if init.nonEmpty)
        yield Assign(Local(javaLocalsSuccessor.ref((locals, i))), dispatch(init.get))
      )

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = e match {
    case AmbiguousThis() => currentThis.head

    case local @ JavaLocal(_) =>
      implicit val o: Origin = local.o

      local.ref.get match {
        case RefAxiomaticDataType(decl) => throw NotAValue(local)
        case RefVariable(decl) => Local(succ[Variable](decl))
        case RefUnloadedJavaNamespace(names) => throw NotAValue(local)
        case RefJavaClass(decl) => throw NotAValue(local)
        case RefJavaField(decls, idx) =>
          if(decls.modifiers.contains(JavaStatic())) {
            Deref(
              obj = FunctionInvocation(javaStaticsFunctionSuccessor.ref(currentJavaClass.head), Nil, Nil)(null),
              ref = javaFieldsSuccessor.ref((decls, idx)),
            )(local.blame)
          } else {
            Deref(currentThis.head, javaFieldsSuccessor.ref((decls, idx)))(local.blame)
          }
        case RefModelField(field) =>
          ModelDeref(currentThis.head, succ[ModelField](field))(local.blame)
        case RefJavaLocalDeclaration(decls, idx) =>
          Local(javaLocalsSuccessor.ref((decls, idx)))
      }

    case local @ PVLLocal(_) =>
      implicit val o: Origin = local.o

      local.ref.get match {
        case RefAxiomaticDataType(decl) => throw NotAValue(local)
        case RefVariable(decl) => Local(succ[Variable](decl))
        case RefModelField(decl) => ModelDeref(currentThis.head, succ[ModelField](decl))(local.blame)
        case RefClass(decl) => throw NotAValue(local)
        case RefField(decl) => Deref(currentThis.head, succ[InstanceField](decl))(local.blame)
      }

    case deref @ JavaDeref(obj, _) =>
      implicit val o: Origin = deref.o

      deref.ref.get match {
        case RefAxiomaticDataType(decl) => throw NotAValue(deref)
        case RefModel(decl) => throw NotAValue(deref)
        case RefJavaClass(decl) => throw NotAValue(deref)
        case RefModelField(decl) => ModelDeref(dispatch(obj), succ[ModelField](decl))(deref.blame)
        case RefUnloadedJavaNamespace(names) => throw NotAValue(deref)
        case RefJavaField(decls, idx) =>
          Deref(dispatch(obj), javaFieldsSuccessor.ref((decls, idx)))(deref.blame)
        case BuiltinField(f) => f(dispatch(obj))
      }

    case deref @ PVLDeref(obj, _) =>
      implicit val o: Origin = deref.o

      deref.ref.get match {
        case RefModelField(decl) => ModelDeref(dispatch(obj), succ[ModelField](decl))(deref.blame)
        case BuiltinField(f) => f(dispatch(obj))
        case RefField(decl) => Deref(dispatch(obj), succ[InstanceField](decl))(deref.blame)
      }

    case JavaLiteralArray(_) => ???

    case inv @ JavaInvocation(obj, typeParams, _, args, _, _) =>
      implicit val o: Origin = inv.o
      inv.ref.get match {
        case RefFunction(decl) =>
          FunctionInvocation(succ[Function](decl), args.map(dispatch), Nil)(null)
        case RefProcedure(decl) =>
          ProcedureInvocation(succ[Procedure](decl), args.map(dispatch), Nil, typeParams.map(dispatch))(null)
        case RefPredicate(decl) =>
          PredicateApply(succ[Predicate](decl), args.map(dispatch))
        case RefInstanceFunction(decl) =>
          InstanceFunctionInvocation(obj.map(dispatch).getOrElse(currentThis.head), succ[InstanceFunction](decl), args.map(dispatch), typeParams.map(dispatch))(null)
        case RefInstanceMethod(decl) =>
          MethodInvocation(obj.map(dispatch).getOrElse(currentThis.head), succ[InstanceMethod](decl), args.map(dispatch), Nil, typeParams.map(dispatch))(null)
        case RefInstancePredicate(decl) =>
          InstancePredicateApply(obj.map(dispatch).getOrElse(currentThis.head), succ[InstancePredicate](decl), args.map(dispatch))
        case RefADTFunction(decl) =>
          ADTFunctionInvocation(None, succ[ADTFunction](decl), args.map(dispatch))
        case RefModelProcess(decl) =>
          ProcessApply(succ[ModelProcess](decl), args.map(dispatch))
        case RefModelAction(decl) =>
          ActionApply(succ[ModelAction](decl), args.map(dispatch))
        case RefJavaMethod(decl) =>
          if(decl.modifiers.contains(JavaStatic())) {
            MethodInvocation(
              obj = FunctionInvocation(javaStaticsFunctionSuccessor.ref(currentJavaClass.head), Nil, Nil)(null),
              ref = succ[InstanceMethod](decl),
              args = args.map(dispatch), outArgs = Nil, typeParams.map(dispatch))(null)
          } else {
            MethodInvocation(
              obj = obj.map(dispatch).getOrElse(currentThis.head),
              ref = succ[InstanceMethod](decl),
              args = args.map(dispatch), outArgs = Nil, typeParams.map(dispatch))(null)
          }
        case BuiltinInstanceMethod(f) =>
          f(dispatch(obj.get))(args.map(dispatch))
      }

    case inv @ PVLInvocation(obj, _, args, typeArgs, givenArgs, yields) =>
      implicit val o: Origin = inv.o

      inv.ref.get match {
        case RefFunction(decl) =>
          FunctionInvocation(succ[Function](decl), args.map(dispatch), typeArgs.map(dispatch))(inv.blame)
        case RefProcedure(decl) =>
          ProcedureInvocation(succ[Procedure](decl), args.map(dispatch), Nil, typeArgs.map(dispatch))(inv.blame)
        case RefPredicate(decl) =>
          PredicateApply(succ[Predicate](decl), args.map(dispatch))
        case RefInstanceFunction(decl) =>
          InstanceFunctionInvocation(
            obj.map(dispatch).getOrElse(currentThis.head),
            succ[InstanceFunction](decl),
            args.map(dispatch),
            typeArgs.map(dispatch))(inv.blame)
        case RefInstanceMethod(decl) =>
          MethodInvocation(obj.map(dispatch).getOrElse(currentThis.head), succ[InstanceMethod](decl), args.map(dispatch), Nil, typeArgs.map(dispatch))(inv.blame)
        case RefInstancePredicate(decl) =>
          InstancePredicateApply(obj.map(dispatch).getOrElse(currentThis.head), succ[InstancePredicate](decl), args.map(dispatch))
        case RefADTFunction(decl) =>
          ADTFunctionInvocation(None, succ[ADTFunction](decl), args.map(dispatch))
        case RefModelProcess(decl) =>
          ProcessApply(succ[ModelProcess](decl), args.map(dispatch))
        case RefModelAction(decl) =>
          ActionApply(succ[ModelAction](decl), args.map(dispatch))
        case BuiltinInstanceMethod(f) =>
          f(dispatch(obj.get))(args.map(dispatch))
      }

    case inv @ JavaNewClass(args, typeParams, t @ JavaTClass(Ref(decl), _)) =>
      implicit val o: Origin = inv.o
      val cons = decl.decls.collectFirst {
        case cons: JavaConstructor if Util.compat(args, cons.parameters) => cons
      }

      val consRef = cons.map(succ[Procedure]).getOrElse(
        new LazyRef[Procedure](successionMap(javaDefaultConstructor(decl))))

      ProcedureInvocation(consRef, args.map(dispatch), Nil, typeParams.map(dispatch))(null)

    case inv @ PVLNew(t @ PVLNamedType(_, _), args) =>
      implicit val o: Origin = inv.o

      t.ref.get match {
        case RefAxiomaticDataType(decl) =>  ???
        case RefModel(decl) => ModelNew(succ[Model](decl))
        case RefClass(decl) =>
          val cons = decl.declarations.collectFirst {
            case cons: PVLConstructor if Util.compat(args, cons.args) => cons
          }

          ProcedureInvocation(
            new LazyRef[Procedure](cons.map(successionMap).getOrElse(pvlDefaultConstructor(decl))),
            args.map(dispatch),
            Nil, Nil,
          )(inv.blame)
      }

    case JavaNewLiteralArray(baseType, dims, initializer) =>
      // Recursively rewrite an array initializer as a list of assignments
      def collectArray(es: JavaLiteralArray, dims: Int, stats: ArrayBuffer[Statement]): Expr = {
        if (dims < 1) throw InvalidArrayInitializerNesting(es)

        implicit val o: Origin = JavaInlineArrayInitializerOrigin(es.o)
        val v = new Variable(FuncTools.repeat(TArray(_), dims, baseType))
        stats += LocalDecl(v)
        es.exprs.zipWithIndex.map {
          case (e: JavaLiteralArray, i) =>
            stats += Assign(AmbiguousSubscript(Local(v.ref), i)(JavaArrayInitializerBlame), collectArray(e, dims-1, stats))
          case (other, i) =>
            stats += Assign(AmbiguousSubscript(Local(v.ref), i)(JavaArrayInitializerBlame), dispatch(other))
        }
        Local(v.ref)
      }

      val stats = ArrayBuffer[Statement]()
      val value = collectArray(initializer match {
        case init: JavaLiteralArray => init
        case _ => throw Unreachable("The top level expression of a JavaNewLiteralArray is never not a LiteralArray.")
      }, dims, stats)
      With(Block(stats.toSeq)(e.o), value)(e.o)

    case JavaNewDefaultArray(t, specified, moreDims) => NewArray(dispatch(t), specified.map(dispatch), moreDims)(e.o)

    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type): Type = t match {
    case JavaTClass(Ref(cls), _) => TClass(javaInstanceClassSuccessor.ref(cls))
    case other => rewriteDefault(other)
  }
}
