package vct.col.newrewrite.lang

import hre.util.{FuncTools, ScopedStack}
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.newrewrite.lang.LangSpecificToCol.{CGlobalStateNotSupported, JavaConstructorOrigin, JavaFieldOrigin, JavaInstanceClassOrigin, JavaLocalOrigin, JavaMethodOrigin, JavaStaticsClassOrigin, ThisVar}
import vct.col.origin._
import vct.col.ref.{LazyRef, Ref}
import vct.col.resolve._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.{Unreachable, UserError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, SuccessionMap}

case object LangSpecificToCol extends RewriterBuilder {
  case class CGlobalStateNotSupported(example: CInit[_]) extends UserError {
    override def code: String = "notSupported"
    override def text: String =
      example.o.messageInContext("Global variables in C are not supported.")
  }

  case object ThisVar extends Origin {
    override def preferredName: String = "this"
    override def context: String = "[At node generated to store the this value for constructors]"
  }

  case class JavaFieldOrigin(fields: JavaFields[_], idx: Int) extends Origin {
    override def preferredName: String = fields.decls(idx)._1
    override def context: String = fields.o.context
  }

  case class JavaLocalOrigin(locals: JavaLocalDeclaration[_], idx: Int) extends Origin {
    override def preferredName: String = locals.decls(idx)._1
    override def context: String = locals.o.context
  }

  case class JavaConstructorOrigin(cons: JavaConstructor[_]) extends Origin {
    override def preferredName: String = cons.name
    override def context: String = cons.o.context
  }

  case class JavaMethodOrigin(method: JavaMethod[_]) extends Origin {
    override def preferredName: String = method.name
    override def context: String = method.o.context
  }

  case class JavaAnnotationMethodOrigin(method: JavaAnnotationMethod[_]) extends Origin {
    override def preferredName: String = method.name
    override def context: String = method.o.context
  }

  case class JavaInstanceClassOrigin(cls: JavaClassOrInterface[_]) extends Origin {
    override def preferredName: String = cls.name
    override def context: String = cls.o.context
  }

  case class JavaStaticsClassOrigin(cls: JavaClassOrInterface[_]) extends Origin {
    override def preferredName: String = cls.name + "Statics"
    override def context: String = cls.o.context
  }

  case class IncompleteTypeArgs(t: SilverPartialADTFunctionInvocation[_]) extends UserError {
    override def code: String = "incompleteTypeArgs"
    override def text: String =
      t.o.messageInContext("This function invocation does not specify all generic types for the domain.")
  }
}

case class LangSpecificToCol[Pre <: Generation]() extends Rewriter[Pre] {
  import LangSpecificToCol._

  case class NotAValue(value: Expr[_]) extends UserError {
    override def code: String = "notAValue"
    override def text: String = value.o.messageInContext("Could not resolve this expression to a value.")
  }

  val namespace: ScopedStack[JavaNamespace[Pre]] = ScopedStack()
  val javaInstanceClassSuccessor: SuccessionMap[JavaClassOrInterface[Pre], Class[Post]] = SuccessionMap()
  val javaStaticsClassSuccessor: SuccessionMap[JavaClassOrInterface[Pre], Class[Post]] = SuccessionMap()
  val javaStaticsFunctionSuccessor: SuccessionMap[JavaClassOrInterface[Pre], Function[Post]] = SuccessionMap()

  val javaFieldsSuccessor: SuccessionMap[(JavaFields[Pre], Int), InstanceField[Post]] = SuccessionMap()
  val javaLocalsSuccessor: SuccessionMap[(JavaLocalDeclaration[Pre], Int), Variable[Post]] = SuccessionMap()

  val javaDefaultConstructor: SuccessionMap[JavaClassOrInterface[Pre], JavaConstructor[Pre]] = SuccessionMap()
  val pvlDefaultConstructor: SuccessionMap[Class[Pre], Procedure[Post]] = SuccessionMap()

  val cFunctionSuccessor: SuccessionMap[CInvocationTarget[Pre], Procedure[Post]] = SuccessionMap()
  val cNameSuccessor: SuccessionMap[CNameTarget[Pre], Variable[Post]] = SuccessionMap()

  val currentThis: ScopedStack[Expr[Post]] = ScopedStack()
  val currentJavaClass: ScopedStack[JavaClassOrInterface[Pre]] = ScopedStack()
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  case class JavaInlineArrayInitializerOrigin(inner: Origin) extends Origin {
    override def preferredName: String = "arrayInitializer"
    override def context: String = inner.context
  }

  case class InvalidArrayInitializerNesting(initializer: JavaLiteralArray[_]) extends UserError {
    override def text: String = initializer.o.messageInContext("This literal array is nested more deeply than its indicated type allows.")
    override def code: String = "invalidNesting"
  }

  def isJavaStatic(decl: ClassDeclaration[_]): Boolean = decl match {
    case init: JavaSharedInitialization[_] => init.isStatic
    case fields: JavaFields[_] => fields.modifiers.collectFirst { case JavaStatic() => () }.nonEmpty
    case method: JavaMethod[_] => method.modifiers.collectFirst { case JavaStatic() => () }.nonEmpty
    case _: JavaConstructor[_] => false
    case _: ClassDeclaration[_] => false // FIXME we should have a way of translating static specification-type declarations
  }

  def makeJavaClass(prefName: String, decls: Seq[ClassDeclaration[Pre]], ref: Ref[Post, Class[Post]])(implicit o: Origin): Unit = {
    // First, declare all the fields, so we can refer to them.
    decls.foreach {
      case fields: JavaFields[Pre] =>
        fields.drop()
        for(((_, dims, _), idx) <- fields.decls.zipWithIndex) {
          javaFieldsSuccessor((fields, idx)) =
            new InstanceField(
              t = FuncTools.repeat(TArray[Post](_), dims, dispatch(fields.t)),
              flags = fields.modifiers.collect { case JavaFinal() => new Final[Post]() }.toSet[FieldFlag[Post]])(JavaFieldOrigin(fields, idx))
          javaFieldsSuccessor((fields, idx)).declareDefault(this)
        }
      case _ =>
    }

    // Each constructor performs in order:
    // 1. the inline initialization of all fields

    val fieldInit = (diz: Expr[Post]) => Block[Post](decls.collect {
      case fields: JavaFields[Pre] =>
        Block(for(((_, _, init), idx) <- fields.decls.zipWithIndex if init.nonEmpty)
          yield assignField[Post](diz, javaFieldsSuccessor.ref((fields, idx)), dispatch(init.get),
            PanicBlame("The inline initialization of a field must have permission, because it is the first initialization that happens."))
        )
    })

    // 2. the shared initialization blocks

    val sharedInit = (diz: Expr[Post]) => {
      currentThis.having(diz) {
        Block(decls.collect {
          case init: JavaSharedInitialization[Pre] => dispatch(init.initialization)
        })
      }
    }

    // 3. the body of the constructor

    val declsDefault = if(decls.collect { case _: JavaConstructor[Pre] => () }.isEmpty) {
      javaDefaultConstructor(currentJavaClass.top) = new JavaConstructor(
        modifiers = Nil,
        name = prefName,
        parameters = Nil,
        typeParameters = Nil,
        signals = Nil,
        body = Block(Nil),
        contract = ApplicableContract(
          requires = UnitAccountedPredicate(tt),
          ensures = UnitAccountedPredicate(AstBuildHelpers.foldStar(decls.collect {
            case fields: JavaFields[Pre] if fields.modifiers.collectFirst { case JavaFinal() => () }.isEmpty =>
              fields.decls.indices.map(decl => {
                val local = JavaLocal[Pre](fields.decls(decl)._1)(DerefPerm)
                local.ref = Some(RefJavaField[Pre](fields, decl))
                Perm(local, WritePerm())
              })
          }.flatten)),
          contextEverywhere = tt, signals = Nil, givenArgs = Nil, yieldsArgs = Nil
        )
      )(PanicBlame("The postcondition of a default constructor cannot fail (but what about commit?)."))
      javaDefaultConstructor(currentJavaClass.top) +: decls
    } else decls

    declsDefault.foreach {
      case cons: JavaConstructor[Pre] =>
        implicit val o: Origin = cons.o
        val t = TClass(ref)
        val resVar = new Variable[Post](t)(ThisVar)
        val res = Local[Post](resVar.ref)
        withResult((result: Result[Post]) =>
          new Procedure(
            returnType = t,
            args = collectInScope(variableScopes) {
              cons.parameters.foreach(dispatch)
            },
            outArgs = Nil, typeArgs = Nil,
            body = currentThis.having(res) { Some(Scope(Seq(resVar), Block(Seq(
              assignLocal(res, NewObject(ref)),
              fieldInit(res),
              sharedInit(res),
              dispatch(cons.body),
              Commit(res)(cons.blame),
              Return(res),
            )))) },
            contract = currentThis.having(result) { cons.contract.rewrite(
              ensures = SplitAccountedPredicate(
                left = UnitAccountedPredicate((result !== Null()) && (TypeOf(result) === TypeValue(t))),
                right = dispatch(cons.contract.ensures),
              ),
              signals = cons.contract.signals.map(dispatch) ++
                cons.signals.map(t => SignalsClause(new Variable(dispatch(t)), tt)),
            ) },
          )(PostBlameSplit.left(PanicBlame("Constructor cannot return null value or value of wrong type."), cons.blame))(JavaConstructorOrigin(cons))
        ).succeedDefault(cons)
      case method: JavaMethod[Pre] =>
        new InstanceMethod(
          returnType = dispatch(method.returnType),
          args = collectInScope(variableScopes) { method.parameters.foreach(dispatch) },
          outArgs = Nil, typeArgs = Nil,
          body = method.modifiers.collectFirst { case sync @ JavaSynchronized() => sync } match {
            case Some(sync) => method.body.map(body => Synchronized(currentThis.top, dispatch(body))(sync.blame))
            case None => method.body.map(dispatch)
          },
          contract = method.contract.rewrite(
            signals = method.contract.signals.map(dispatch) ++
              method.signals.map(t => SignalsClause(new Variable(dispatch(t)), tt)),
          ),
        )(method.blame)(JavaMethodOrigin(method)).succeedDefault(method)
      case method: JavaAnnotationMethod[Pre] =>
        new InstanceMethod(
          returnType = dispatch(method.returnType),
          args = Nil,
          outArgs = Nil, typeArgs = Nil,
          body = None,
          contract = contract()
        )(PanicBlame("Verification of annotation method cannot fail"))(JavaAnnotationMethodOrigin(method)).succeedDefault(method)
      case _: JavaSharedInitialization[Pre] =>
      case _: JavaFields[Pre] =>
      case other => dispatch(other)
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case model: Model[Pre] =>
      implicit val o: Origin = model.o
      currentThis.having(ThisModel[Post](succ(model))) {
        model.rewrite().succeedDefault(model)
      }

    case ns: JavaNamespace[Pre] =>
      ns.drop()
      namespace.having(ns) {
        // Do not enter a scope, so classes of the namespace are declared to the program.
        ns.declarations.foreach(dispatch)
      }

    case cls: JavaClassOrInterface[Pre] =>
      implicit val o: Origin = cls.o

      currentJavaClass.having(cls) {
        val supports = cls.supports.map(dispatch).flatMap {
          case TClass(ref) => Seq(ref)
          case _ => ???
        }

        val instDecls = cls.decls.filter(!isJavaStatic(_))
        val staticDecls = cls.decls.filter(isJavaStatic)

        val lockInvariant = cls match {
          case clazz: JavaClass[Pre] => clazz.intrinsicLockInvariant
          case _: JavaInterface[Pre] => tt[Pre]
          case _: JavaAnnotationInterface[Pre] => tt[Pre]
        }

        val instanceClass = currentThis.having(ThisObject(javaInstanceClassSuccessor.ref(cls))) {
          new Class[Post](collectInScope(classScopes) {
            makeJavaClass(cls.name, instDecls, javaInstanceClassSuccessor.ref(cls))
          }, supports, dispatch(lockInvariant))(JavaInstanceClassOrigin(cls))
        }

        instanceClass.declareDefault(this)
        javaInstanceClassSuccessor(cls) = instanceClass

        if(staticDecls.nonEmpty) {
          val staticsClass = new Class[Post](collectInScope(classScopes) {
            currentThis.having(ThisObject(javaStaticsClassSuccessor.ref(cls))) {
              makeJavaClass(cls.name + "Statics", staticDecls, javaStaticsClassSuccessor.ref(cls))
            }
          }, Nil, tt)(JavaStaticsClassOrigin(cls))

          staticsClass.declareDefault(this)
          val t = TClass[Post](staticsClass.ref)
          val singleton = withResult((res: Result[Post]) =>
            function(AbstractApplicable, returnType = t,
              ensures = UnitAccountedPredicate((res !== Null()) && (TypeOf(res) === TypeValue(t)))))
          singleton.declareDefault(this)
          javaStaticsClassSuccessor(cls) = staticsClass
          javaStaticsFunctionSuccessor(cls) = singleton
        }
      }

    case cons: PVLConstructor[Pre] =>
      implicit val o: Origin = cons.o
      val t = TClass[Post](succ(currentClass.top))
      val resVar = new Variable(t)
      withResult((result: Result[Post]) => new Procedure[Post](
        returnType = t,
        args = collectInScope(variableScopes) { cons.args.foreach(dispatch) },
        outArgs = Nil,
        typeArgs = Nil,
        body = currentThis.having(resVar.get) { cons.body.map(body => Scope(Seq(resVar), Block(Seq(
          assignLocal(resVar.get, NewObject[Post](succ(currentClass.top))),
          dispatch(body),
          Commit(resVar.get)(cons.blame),
          Return(resVar.get),
        )))) },
        contract = currentThis.having(result) { cons.contract.rewrite(
          ensures = SplitAccountedPredicate(
            left = UnitAccountedPredicate((result !== Null()) && (TypeOf(result) === TypeValue(t))),
            right = dispatch(cons.contract.ensures),
          )
        ) },
      )(PostBlameSplit.left(PanicBlame("Constructor cannot return null value or value of wrong type."), cons.blame))).succeedDefault(cons)

    case cParam: CParam[Pre] =>
      cParam.drop()
      val v = new Variable[Post](cParam.specifiers.collectFirst { case t: CSpecificationType[Pre] => dispatch(t.t) }.getOrElse(???))(cParam.o)
      cNameSuccessor(RefCParam(cParam)) = v
      v.declareDefault(this)

    case func: CFunctionDefinition[Pre] =>
      func.drop()
      val info = C.getDeclaratorInfo(func.declarator)
      val returnType = func.specs.collectFirst { case t: CSpecificationType[Pre] => dispatch(t.t) }.getOrElse(???)
      val params = collectInScope(variableScopes) { info.params.get.foreach(dispatch) }
      cFunctionSuccessor(RefCFunctionDefinition(func)) = new Procedure(
        returnType = returnType,
        args = params,
        outArgs = Nil,
        typeArgs = Nil,
        body = Some(dispatch(func.body)),
        contract = contract()(func.o),
      )(func.blame)(func.o).declareDefault(this)

    case decl: CGlobalDeclaration[Pre] =>
      val t = decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] => dispatch(t.t) }.getOrElse(???)
      for((init, idx) <- decl.decl.inits.zipWithIndex) {
        val info = C.getDeclaratorInfo(init.decl)
        info.params match {
          case Some(params) =>
            cFunctionSuccessor(RefCGlobalDeclaration(decl, idx)) = new Procedure[Post](
              returnType = t,
              args = collectInScope(variableScopes) { params.foreach(dispatch) },
              outArgs = Nil,
              typeArgs = Nil,
              body = None,
              contract = contract()(init.o),
            )(AbstractApplicable)(init.o).declareDefault(this)
          case None =>
            throw CGlobalStateNotSupported(init)
        }
      }

    case decl: CDeclaration[Pre] => ???

    case cls: Class[Pre] =>
      currentClass.having(cls) {
        currentThis.having(ThisObject[Post](succ(cls))(cls.o)) {
          val decls = collectInScope(classScopes) {
            cls.declarations.foreach(dispatch)

            if (cls.declarations.collectFirst { case _: PVLConstructor[Pre] => () }.isEmpty) {
              implicit val o: Origin = cls.o
              val t = TClass[Post](succ(cls))
              val resVar = new Variable[Post](t)
              val res = Local[Post](resVar.ref)(ThisVar)
              val defaultBlame = PanicBlame("The postcondition of a default constructor cannot fail (but what about commit?).")

              pvlDefaultConstructor(cls) = withResult((result: Result[Post]) => new Procedure(
                t,
                Nil, Nil, Nil,
                Some(Scope(Seq(resVar), Block(Seq(
                  assignLocal(res, NewObject[Post](succ(cls))),
                  Commit(res)(defaultBlame),
                  Return(res),
                )))),
                ApplicableContract(
                  UnitAccountedPredicate(tt),
                  UnitAccountedPredicate(AstBuildHelpers.foldStar(cls.declarations.collect {
                    case field: InstanceField[Pre] =>
                      fieldPerm[Post](result, succ(field), WritePerm())
                  })), tt, Nil, Nil, Nil,
                )
              )(defaultBlame))

              pvlDefaultConstructor(cls).declareDefault(this)
            }
          }

          cls.rewrite(decls).succeedDefault(cls)
        }
      }

    case other => rewriteDefault(other)
  }


  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case scope @ Scope(locals, body) =>
      def scanScope(node: Node[Pre]): Unit = node match {
        case Scope(_, _) =>
        case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) =>
          locals.drop()
          implicit val o: Origin = node.o
          locals.decls.zipWithIndex.foreach {
            case ((_, dims, _), idx) =>
              val v = new Variable[Post](FuncTools.repeat(TArray[Post](_), dims, dispatch(locals.t)))(JavaLocalOrigin(locals, idx))
              javaLocalsSuccessor((locals, idx)) = v
              v.declareDefault(this)
          }
        case other => other.subnodes.foreach(scanScope)
      }

      scope.rewrite(locals = collectInScope(variableScopes) {
        locals.foreach(dispatch)
        scanScope(body)
      })

    case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) =>
      implicit val o: Origin = locals.o
      Block(for(((_, _, init), i) <- locals.decls.zipWithIndex if init.nonEmpty)
        yield assignLocal(Local(javaLocalsSuccessor.ref((locals, i))), dispatch(init.get))
      )

    case CDeclarationStatement(decl) =>
      decl.drop()
      // PB: this is correct because Seq[CInit]'s are flattened, but the structure is a bit stupid.
      val t = decl.specs.collectFirst { case t: CSpecificationType[Pre] => dispatch(t.t) }.getOrElse(???)
      Block(for((init, idx) <- decl.inits.zipWithIndex) yield {
        val info = C.getDeclaratorInfo(init.decl)
        info.params match {
          case Some(params) => ???
          case None =>
            val v = new Variable[Post](t)(init.o)
            cNameSuccessor(RefCDeclaration(decl, idx)) = v
            implicit val o: Origin = init.o
            init.init match {
              case Some(value) =>
                Block(Seq(LocalDecl(v), assignLocal(v.get, dispatch(value))))
              case None => LocalDecl(v)
            }
        }
      })(decl.o)

    case goto: CGoto[Pre] =>
      Goto[Post](succ(goto.ref.getOrElse(???)))(goto.o)

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case result @ AmbiguousResult() =>
      implicit val o: Origin = result.o
      result.ref.get match {
        case ref: RefCFunctionDefinition[Pre] =>
          Result[Post](cFunctionSuccessor.ref(ref))
        case ref: RefCGlobalDeclaration[Pre] =>
          Result[Post](cFunctionSuccessor.ref(ref))
        case RefFunction(decl) => Result(succ(decl))
        case RefProcedure(decl) => Result(succ(decl))
        case RefJavaMethod(decl) => Result(succ(decl))
        case RefInstanceFunction(decl) => Result(succ(decl))
        case RefInstanceMethod(decl) => Result(succ(decl))
      }

    case diz @ AmbiguousThis() =>
      currentThis.top

    case local @ JavaLocal(_) =>
      implicit val o: Origin = local.o

      local.ref.get match {
        case RefAxiomaticDataType(decl) => throw NotAValue(local)
        case RefVariable(decl) => Local(succ(decl))
        case RefUnloadedJavaNamespace(names) => throw NotAValue(local)
        case RefJavaClass(decl) => throw NotAValue(local)
        case RefJavaField(decls, idx) =>
          if(decls.modifiers.contains(JavaStatic())) {
            Deref[Post](
              obj = FunctionInvocation[Post](javaStaticsFunctionSuccessor.ref(currentJavaClass.top), Nil, Nil, Nil, Nil)(PanicBlame("Statics singleton function requires nothing.")),
              ref = javaFieldsSuccessor.ref((decls, idx)),
            )(local.blame)
          } else {
            Deref[Post](currentThis.top, javaFieldsSuccessor.ref((decls, idx)))(local.blame)
          }
        case RefModelField(field) =>
          ModelDeref[Post](currentThis.top, succ(field))(local.blame)
        case RefJavaLocalDeclaration(decls, idx) =>
          Local(javaLocalsSuccessor.ref((decls, idx)))
      }

    case local @ PVLLocal(_) =>
      implicit val o: Origin = local.o

      local.ref.get match {
        case RefAxiomaticDataType(decl) => throw NotAValue(local)
        case RefVariable(decl) => Local(succ(decl))
        case RefModelField(decl) => ModelDeref[Post](currentThis.top, succ(decl))(local.blame)
        case RefClass(decl) => throw NotAValue(local)
        case RefField(decl) => Deref[Post](currentThis.top, succ(decl))(local.blame)
      }

    case local @ CLocal(_) =>
      implicit val o: Origin = local.o

      local.ref.get match {
        case RefAxiomaticDataType(decl) => throw NotAValue(local)
        case RefVariable(decl) => Local(succ(decl))
        case RefModelField(decl) => ModelDeref[Post](currentThis.top, succ(decl))(local.blame)
        case ref: RefCParam[Pre] => Local(cNameSuccessor.ref(ref))
        case RefCFunctionDefinition(decl) => throw NotAValue(local)
        case RefCGlobalDeclaration(decls, initIdx) => throw NotAValue(local)
        case ref: RefCDeclaration[Pre] => Local(cNameSuccessor.ref(ref))
      }

    case deref @ JavaDeref(obj, _) =>
      implicit val o: Origin = deref.o

      deref.ref.get match {
        case RefAxiomaticDataType(decl) => throw NotAValue(deref)
        case RefModel(decl) => throw NotAValue(deref)
        case RefJavaClass(decl) => throw NotAValue(deref)
        case RefModelField(decl) => ModelDeref[Post](dispatch(obj), succ(decl))(deref.blame)
        case RefUnloadedJavaNamespace(names) => throw NotAValue(deref)
        case RefJavaField(decls, idx) =>
          Deref[Post](dispatch(obj), javaFieldsSuccessor.ref((decls, idx)))(deref.blame)
        case BuiltinField(f) => dispatch(f(obj))
        case RefVariable(v) => ???
      }

    case deref @ PVLDeref(obj, _) =>
      implicit val o: Origin = deref.o

      deref.ref.get match {
        case RefModelField(decl) => ModelDeref[Post](dispatch(obj), succ(decl))(deref.blame)
        case BuiltinField(f) => dispatch(f(obj))
        case RefField(decl) => Deref[Post](dispatch(obj), succ(decl))(deref.blame)
      }

    case JavaLiteralArray(_) => ???

    case inv @ JavaInvocation(obj, typeParams, _, args, givenMap, yields) =>
      implicit val o: Origin = inv.o
      inv.ref.get match {
        case RefFunction(decl) =>
          FunctionInvocation[Post](succ(decl), args.map(dispatch), Nil,
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefProcedure(decl) =>
          ProcedureInvocation[Post](succ(decl), args.map(dispatch), Nil, typeParams.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefPredicate(decl) =>
          PredicateApply[Post](succ(decl), args.map(dispatch), WritePerm())
        case RefInstanceFunction(decl) =>
          InstanceFunctionInvocation[Post](obj.map(dispatch).getOrElse(currentThis.top), succ(decl), args.map(dispatch), typeParams.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefInstanceMethod(decl) =>
          MethodInvocation[Post](obj.map(dispatch).getOrElse(currentThis.top), succ(decl), args.map(dispatch), Nil, typeParams.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefInstancePredicate(decl) =>
          InstancePredicateApply[Post](obj.map(dispatch).getOrElse(currentThis.top), succ(decl), args.map(dispatch), WritePerm())
        case RefADTFunction(decl) =>
          ADTFunctionInvocation[Post](None, succ(decl), args.map(dispatch))
        case RefModelProcess(decl) =>
          ProcessApply[Post](succ(decl), args.map(dispatch))
        case RefModelAction(decl) =>
          ActionApply[Post](succ(decl), args.map(dispatch))
        case RefJavaMethod(decl) =>
          if(decl.modifiers.contains(JavaStatic[Pre]())) {
            MethodInvocation[Post](
              obj = FunctionInvocation[Post](javaStaticsFunctionSuccessor.ref(currentJavaClass.top), Nil, Nil, Nil, Nil)(inv.blame),
              ref = succ(decl),
              args = args.map(dispatch), outArgs = Nil, typeParams.map(dispatch),
              givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
              yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
            )(inv.blame)
          } else {
            MethodInvocation[Post](
              obj = obj.map(dispatch).getOrElse(currentThis.top),
              ref = succ(decl),
              args = args.map(dispatch), outArgs = Nil, typeParams.map(dispatch),
              givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
              yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
            )(inv.blame)
          }
        case RefJavaAnnotationMethod(decl) =>
          MethodInvocation[Post](
            obj = obj.map(dispatch).getOrElse(currentThis.top),
            ref = succ(decl),
            args = Nil, outArgs = Nil, Nil, Nil, Nil
          )(inv.blame)
        case BuiltinInstanceMethod(f) =>
          dispatch(f(obj.get)(args))
      }

    case inv @ PVLInvocation(obj, _, args, typeArgs, givenMap, yields) =>
      implicit val o: Origin = inv.o

      inv.ref.get match {
        case RefFunction(decl) =>
          FunctionInvocation[Post](succ(decl), args.map(dispatch), typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefProcedure(decl) =>
          ProcedureInvocation[Post](succ(decl), args.map(dispatch), Nil, typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefPredicate(decl) =>
          PredicateApply[Post](succ(decl), args.map(dispatch), WritePerm())
        case RefInstanceFunction(decl) =>
          InstanceFunctionInvocation[Post](
            obj.map(dispatch).getOrElse(currentThis.top),
            succ(decl),
            args.map(dispatch),
            typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) },
          )(inv.blame)
        case RefInstanceMethod(decl) =>
          MethodInvocation[Post](obj.map(dispatch).getOrElse(currentThis.top), succ(decl), args.map(dispatch), Nil, typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefInstancePredicate(decl) =>
          InstancePredicateApply[Post](obj.map(dispatch).getOrElse(currentThis.top), succ(decl), args.map(dispatch), WritePerm())
        case RefADTFunction(decl) =>
          ADTFunctionInvocation[Post](None, succ(decl), args.map(dispatch))
        case RefModelProcess(decl) =>
          ProcessApply[Post](succ(decl), args.map(dispatch))
        case RefModelAction(decl) =>
          ActionApply[Post](succ(decl), args.map(dispatch))
        case BuiltinInstanceMethod(f) =>
          dispatch(f(obj.get)(args))
      }

    case inv @ CInvocation(applicable, args, givenMap, yields) =>
      implicit val o: Origin = inv.o
      inv.ref.get match {
        case RefFunction(decl) =>
          FunctionInvocation[Post](succ(decl), args.map(dispatch), Nil,
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefProcedure(decl) =>
          ProcedureInvocation[Post](succ(decl), args.map(dispatch), Nil, Nil,
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefPredicate(decl) =>
          PredicateApply[Post](succ(decl), args.map(dispatch), WritePerm())
        case RefInstanceFunction(decl) => ???
        case RefInstanceMethod(decl) => ???
        case RefInstancePredicate(decl) => ???
        case RefADTFunction(decl) =>
          ADTFunctionInvocation[Post](None, succ(decl), args.map(dispatch))
        case RefModelProcess(decl) =>
          ProcessApply[Post](succ(decl), args.map(dispatch))
        case RefModelAction(decl) =>
          ActionApply[Post](succ(decl), args.map(dispatch))
        case BuiltinInstanceMethod(f) => ???
        case ref: RefCFunctionDefinition[Pre] =>
          ProcedureInvocation[Post](cFunctionSuccessor.ref(ref), args.map(dispatch), Nil, Nil,
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case RefCGlobalDeclaration(decls, initIdx) => ???
        case RefCDeclaration(decls, initIdx) => ???
      }

    case inv @ JavaNewClass(args, typeParams, t @ JavaTClass(Ref(decl), _), givenMap, yields) =>
      implicit val o: Origin = inv.o

      inv.ref.get match {
        case RefModel(decl) => ModelNew[Post](succ(decl))
        case RefJavaConstructor(cons) =>
          ProcedureInvocation[Post](succ[Procedure[Post]](cons), args.map(dispatch), Nil, typeParams.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case ImplicitDefaultJavaConstructor() =>
          ProcedureInvocation[Post](new LazyRef[Post, Procedure[Post]](successionMap.top(javaDefaultConstructor(decl))),
            args.map(dispatch), Nil, typeParams.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
      }

    case inv @ PVLNew(t, args, givenMap, yields) =>
      implicit val o: Origin = inv.o

      inv.ref.get match {
        case RefModel(decl) => ModelNew[Post](succ(decl))
        case RefPVLConstructor(decl) =>
          ProcedureInvocation[Post](succ(decl), args.map(dispatch), Nil, Nil,
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
        case ImplicitDefaultPVLConstructor() =>
          ProcedureInvocation[Post](pvlDefaultConstructor.ref(t.asInstanceOf[TClass[Pre]].cls.decl), args.map(dispatch), Nil, Nil,
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)
      }

    case JavaNewLiteralArray(baseType, dims, initializer) =>
      // Recursively rewrite an array initializer as a list of assignments
      def collectArray(es: JavaLiteralArray[Pre], dims: Int, stats: ArrayBuffer[Statement[Post]]): Expr[Post] = {
        if (dims < 1) throw InvalidArrayInitializerNesting(es)

        implicit val o: Origin = JavaInlineArrayInitializerOrigin(es.o)
        val v = new Variable[Post](FuncTools.repeat[Type[Post]](TArray[Post](_), dims, dispatch(baseType)))
        stats += LocalDecl(v)
        stats += assignLocal(v.get, NewArray(FuncTools.repeat[Type[Post]](TArray[Post](_), dims-1, dispatch(baseType)), Seq(const(es.exprs.size)), 0))
        es.exprs.zipWithIndex.map {
          case (e: JavaLiteralArray[Pre], i) =>
            stats += Assign(AmbiguousSubscript(Local[Post](v.ref), const(i))(JavaArrayInitializerBlame), collectArray(e, dims-1, stats))(
              PanicBlame("Assignment for an explicit array initializer cannot fail."))
          case (other, i) =>
            stats += Assign(AmbiguousSubscript(Local[Post](v.ref), const(i))(JavaArrayInitializerBlame), dispatch(other))(
              PanicBlame("Assignment for an explicit array initializer cannot fail."))
        }
        Local[Post](v.ref)
      }

      val stats = ArrayBuffer[Statement[Post]]()
      val value = collectArray(initializer match {
        case init: JavaLiteralArray[Pre] => init
        case _ => throw Unreachable("The top level expression of a JavaNewLiteralArray is never not a LiteralArray.")
      }, dims, stats)
      With(Block(stats.toSeq)(e.o), value)(e.o)

    case JavaNewDefaultArray(t, specified, moreDims) => NewArray(dispatch(t), specified.map(dispatch), moreDims)(e.o)

    case inv @ SilverPartialADTFunctionInvocation(_, args, _) =>
      inv.maybeTypeArgs match {
        case None => throw IncompleteTypeArgs(inv)
        case Some(typeArgs) =>
          ADTFunctionInvocation[Post](Some(succ(inv.adt), typeArgs.map(dispatch)), succ(inv.function), args.map(dispatch))(inv.o)
      }

    case map @ SilverUntypedNonemptyLiteralMap(values) =>
      LiteralMap(dispatch(map.keyType), dispatch(map.valueType), values.map { case (k, v) => (dispatch(k), dispatch(v)) })(map.o)

    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case JavaTClass(Ref(cls), _) => TClass(javaInstanceClassSuccessor.ref(cls))
    case other => rewriteDefault(other)
  }
}
