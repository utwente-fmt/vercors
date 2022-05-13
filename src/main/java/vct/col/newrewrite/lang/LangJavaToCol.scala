package vct.col.newrewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.{FuncTools, ScopedStack}
import vct.col.ast._
import vct.col.newrewrite.lang.LangSpecificToCol.{NotAValue, ThisVar}
import vct.col.origin.{AbstractApplicable, DerefPerm, JavaArrayInitializerBlame, Origin, PanicBlame, PostBlameSplit, TrueSatisfiable}
import vct.col.ref.{LazyRef, Ref}
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, ImplicitDefaultJavaConstructor, RefADTFunction, RefAxiomaticDataType, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefJavaAnnotationMethod, RefJavaClass, RefJavaConstructor, RefJavaField, RefJavaLocalDeclaration, RefJavaMethod, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefUnloadedJavaNamespace, RefVariable}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import RewriteHelpers._
import vct.result.VerificationError.UserError

import scala.collection.mutable

case object LangJavaToCol {
  case class JavaFieldOrigin(fields: JavaFields[_], idx: Int) extends Origin {
    override def preferredName: String = fields.decls(idx).name
    override def shortPosition: String = fields.decls(idx).o.shortPosition
    override def context: String = fields.o.context
    override def inlineContext: String = fields.decls(idx).o.inlineContext
  }

  case class JavaLocalOrigin(locals: JavaLocalDeclaration[_], idx: Int) extends Origin {
    override def preferredName: String = locals.decls(idx).name
    override def shortPosition: String = locals.decls(idx).o.shortPosition
    override def context: String = locals.o.context
    override def inlineContext: String = locals.decls(idx).o.inlineContext
  }

  case class JavaConstructorOrigin(cons: JavaConstructor[_]) extends Origin {
    override def preferredName: String = cons.name
    override def shortPosition: String = cons.o.shortPosition
    override def context: String = cons.o.context
    override def inlineContext: String = cons.o.inlineContext
  }

  case class JavaMethodOrigin(method: JavaMethod[_]) extends Origin {
    override def preferredName: String = method.name
    override def shortPosition: String = method.o.shortPosition
    override def context: String = method.o.context
    override def inlineContext: String = method.o.inlineContext
  }

  case class JavaAnnotationMethodOrigin(method: JavaAnnotationMethod[_]) extends Origin {
    override def preferredName: String = method.name
    override def shortPosition: String = method.o.shortPosition
    override def context: String = method.o.context
    override def inlineContext: String = method.o.inlineContext
  }

  case class JavaInstanceClassOrigin(cls: JavaClassOrInterface[_]) extends Origin {
    override def preferredName: String = cls.name
    override def shortPosition: String = cls.o.shortPosition
    override def context: String = cls.o.context
    override def inlineContext: String = cls.o.inlineContext
  }

  case class JavaStaticsClassOrigin(cls: JavaClassOrInterface[_]) extends Origin {
    override def preferredName: String = cls.name + "Statics"
    override def shortPosition: String = cls.o.shortPosition
    override def context: String = cls.o.context
    override def inlineContext: String = cls.o.inlineContext
  }

  case class JavaStaticsClassSingletonOrigin(cls: JavaClassOrInterface[_]) extends Origin {
    override def preferredName: String = cls.name + "StaticsSingleton"
    override def shortPosition: String = cls.o.shortPosition
    override def context: String = cls.o.context
    override def inlineContext: String = cls.o.inlineContext
  }

  case class JavaInlineArrayInitializerOrigin(inner: Origin) extends Origin {
    override def preferredName: String = "arrayInitializer"
    override def shortPosition: String = inner.shortPosition
    override def context: String = inner.context
    override def inlineContext: String = inner.inlineContext
  }

  case class InvalidArrayInitializerNesting(initializer: JavaLiteralArray[_]) extends UserError {
    override def text: String = initializer.o.messageInContext("This literal array is nested more deeply than its indicated type allows.")
    override def code: String = "invalidNesting"
  }
}

case class LangJavaToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  import LangJavaToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val namespace: ScopedStack[JavaNamespace[Pre]] = ScopedStack()
  val javaInstanceClassSuccessor: SuccessionMap[JavaClassOrInterface[Pre], Class[Post]] = SuccessionMap()
  val javaStaticsClassSuccessor: SuccessionMap[JavaClassOrInterface[Pre], Class[Post]] = SuccessionMap()
  val javaStaticsFunctionSuccessor: SuccessionMap[JavaClassOrInterface[Pre], Function[Post]] = SuccessionMap()

  val javaFieldsSuccessor: SuccessionMap[(JavaFields[Pre], Int), InstanceField[Post]] = SuccessionMap()
  val javaLocalsSuccessor: SuccessionMap[(JavaLocalDeclaration[Pre], Int), Variable[Post]] = SuccessionMap()

  val javaDefaultConstructor: SuccessionMap[JavaClassOrInterface[Pre], JavaConstructor[Pre]] = SuccessionMap()

  val javaClassDeclToJavaClass: mutable.Map[JavaClassDeclaration[Pre], JavaClassOrInterface[Pre]] = mutable.Map()

  val currentJavaClass: ScopedStack[JavaClassOrInterface[Pre]] = ScopedStack()

  def isJavaStatic(decl: ClassDeclaration[_]): Boolean = decl match {
    case init: JavaSharedInitialization[_] => init.isStatic
    case fields: JavaFields[_] => fields.modifiers.collectFirst { case JavaStatic() => () }.nonEmpty
    case method: JavaMethod[_] => method.modifiers.collectFirst { case JavaStatic() => () }.nonEmpty
    case _: JavaConstructor[_] => false
    case _: ClassDeclaration[_] => false // FIXME we should have a way of translating static specification-type declarations
  }

  def makeJavaClass(prefName: String, decls: Seq[ClassDeclaration[Pre]], ref: Ref[Post, Class[Post]], wantDefaultConstructor: Boolean)(implicit o: Origin): Unit = {
    // First, declare all the fields, so we can refer to them.
    decls.foreach {
      case fields: JavaFields[Pre] =>
        fields.drop()
        for((JavaVariableDeclaration(_, dims, _), idx) <- fields.decls.zipWithIndex) {
          javaFieldsSuccessor((fields, idx)) =
            new InstanceField(
              t = FuncTools.repeat(TArray[Post](_), dims, rw.dispatch(fields.t)),
              flags = fields.modifiers.collect { case JavaFinal() => new Final[Post]() }.toSet[FieldFlag[Post]])(JavaFieldOrigin(fields, idx))
          javaFieldsSuccessor((fields, idx)).declareDefault(rw)
        }
      case _ =>
    }

    // Each constructor performs in order:
    // 1. the inline initialization of all fields

    val fieldInit = (diz: Expr[Post]) => Block[Post](decls.collect {
      case fields: JavaFields[Pre] =>
        Block(for((JavaVariableDeclaration(_, _, init), idx) <- fields.decls.zipWithIndex if init.nonEmpty)
          yield assignField[Post](diz, javaFieldsSuccessor.ref((fields, idx)), rw.dispatch(init.get),
            PanicBlame("The inline initialization of a field must have permission, because it is the first initialization that happens."))
        )
    })

    // 2. the shared initialization blocks

    val sharedInit = (diz: Expr[Post]) => {
      rw.currentThis.having(diz) {
        Block(decls.collect {
          case init: JavaSharedInitialization[Pre] => rw.dispatch(init.initialization)
        })
      }
    }

    // 3. the body of the constructor

    val declsDefault = if(wantDefaultConstructor && decls.collect { case _: JavaConstructor[Pre] => () }.isEmpty) {
      javaDefaultConstructor(currentJavaClass.top) = new JavaConstructor(
        modifiers = Nil,
        name = prefName,
        parameters = Nil,
        typeParameters = Nil,
        signals = Nil,
        body = Block(Nil),
        contract = ApplicableContract(
          requires = UnitAccountedPredicate(tt),
          ensures = UnitAccountedPredicate(foldStar(decls.collect {
            case fields: JavaFields[Pre] if fields.modifiers.collectFirst { case JavaFinal() => () }.isEmpty =>
              fields.decls.indices.map(decl => {
                val local = JavaLocal[Pre](fields.decls(decl).name)(DerefPerm)
                local.ref = Some(RefJavaField[Pre](fields, decl))
                Perm(local, WritePerm())
              })
          }.flatten)),
          contextEverywhere = tt, signals = Nil, givenArgs = Nil, yieldsArgs = Nil
        )(TrueSatisfiable)
      )(PanicBlame("The postcondition of a default constructor cannot fail (but what about commit?)."))
      javaDefaultConstructor(currentJavaClass.top) +: decls
    } else decls

    declsDefault.foreach {
      case cons: JavaConstructor[Pre] =>
        logger.debug(s"Constructor for ${cons.o.context}")
        implicit val o: Origin = cons.o
        val t = TClass(ref)
        val resVar = new Variable[Post](t)(ThisVar)
        val res = Local[Post](resVar.ref)
        withResult((result: Result[Post]) =>
          new Procedure(
            returnType = t,
            args = rw.collectInScope(rw.variableScopes) {
              cons.parameters.foreach(rw.dispatch)
            },
            outArgs = Nil, typeArgs = Nil,
            body = rw.currentThis.having(res) { Some(Scope(Seq(resVar), Block(Seq(
              assignLocal(res, NewObject(ref)),
              fieldInit(res),
              sharedInit(res),
              rw.dispatch(cons.body),
              Commit(res)(cons.blame),
              Return(res),
            )))) },
            contract = rw.currentThis.having(result) { cons.contract.rewrite(
              ensures = SplitAccountedPredicate(
                left = UnitAccountedPredicate((result !== Null()) && (TypeOf(result) === TypeValue(t))),
                right = rw.dispatch(cons.contract.ensures),
              ),
              signals = cons.contract.signals.map(rw.dispatch) ++
                cons.signals.map(t => SignalsClause(new Variable(rw.dispatch(t)), tt)),
            ) },
          )(PostBlameSplit.left(PanicBlame("Constructor cannot return null value or value of wrong type."), cons.blame))(JavaConstructorOrigin(cons))
        ).succeedDefault(cons)
      case method: JavaMethod[Pre] =>
        new InstanceMethod(
          returnType = rw.dispatch(method.returnType),
          args = rw.collectInScope(rw.variableScopes) { method.parameters.foreach(rw.dispatch) },
          outArgs = Nil, typeArgs = Nil,
          body = method.modifiers.collectFirst { case sync @ JavaSynchronized() => sync } match {
            case Some(sync) => method.body.map(body => Synchronized(rw.currentThis.top, rw.dispatch(body))(sync.blame))
            case None => method.body.map(rw.dispatch)
          },
          contract = method.contract.rewrite(
            signals = method.contract.signals.map(rw.dispatch) ++
              method.signals.map(t => SignalsClause(new Variable(rw.dispatch(t)), tt)),
          ),
        )(method.blame)(JavaMethodOrigin(method)).succeedDefault(method)
      case method: JavaAnnotationMethod[Pre] =>
        new InstanceMethod(
          returnType = rw.dispatch(method.returnType),
          args = Nil,
          outArgs = Nil, typeArgs = Nil,
          body = None,
          contract = contract(TrueSatisfiable)
        )(PanicBlame("Verification of annotation method cannot fail"))(JavaAnnotationMethodOrigin(method)).succeedDefault(method)
      case _: JavaSharedInitialization[Pre] =>
      case _: JavaFields[Pre] =>
      case other => rw.dispatch(other)
    }
  }

  def rewriteClass(cls: JavaClassOrInterface[Pre]): Unit = {
    implicit val o: Origin = cls.o

    cls.decls.collect({
      case decl: JavaClassDeclaration[Pre] =>
        javaClassDeclToJavaClass(decl) = cls
    })

    currentJavaClass.having(cls) {
      val supports = cls.supports.map(rw.dispatch).flatMap {
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

      val instanceClass = rw.currentThis.having(ThisObject(javaInstanceClassSuccessor.ref(cls))) {
        new Class[Post](rw.collectInScope(rw.classScopes) {
          makeJavaClass(cls.name, instDecls, javaInstanceClassSuccessor.ref(cls), wantDefaultConstructor = true)
        }, supports, rw.dispatch(lockInvariant))(JavaInstanceClassOrigin(cls))
      }

      instanceClass.declareDefault(rw)
      javaInstanceClassSuccessor(cls) = instanceClass

      if(staticDecls.nonEmpty) {
        val staticsClass = new Class[Post](rw.collectInScope(rw.classScopes) {
          rw.currentThis.having(ThisObject(javaStaticsClassSuccessor.ref(cls))) {
            makeJavaClass(cls.name + "Statics", staticDecls, javaStaticsClassSuccessor.ref(cls), wantDefaultConstructor = false)
          }
        }, Nil, tt)(JavaStaticsClassOrigin(cls))

        staticsClass.declareDefault(rw)
        val t = TClass[Post](staticsClass.ref)
        val singleton = withResult((res: Result[Post]) =>
          function(AbstractApplicable, TrueSatisfiable, returnType = t,
            ensures = UnitAccountedPredicate((res !== Null()) && (TypeOf(res) === TypeValue(t))))(JavaStaticsClassSingletonOrigin(cls)))
        singleton.declareDefault(rw)
        javaStaticsClassSuccessor(cls) = staticsClass
        javaStaticsFunctionSuccessor(cls) = singleton
      }
    }
  }

  def rewriteNamespace(ns: JavaNamespace[Pre]): Unit = {
    ns.drop()
    namespace.having(ns) {
      // Do not enter a scope, so classes of the namespace are declared to the program.
      ns.declarations.foreach(rw.dispatch)
    }
  }

  def declareLocal(locals: JavaLocalDeclaration[Pre]): Unit = {
    locals.drop()
    implicit val o: Origin = locals.o
    locals.decls.zipWithIndex.foreach {
      case (JavaVariableDeclaration(_, dims, _), idx) =>
        val v = new Variable[Post](FuncTools.repeat(TArray[Post](_), dims, rw.dispatch(locals.t)))(JavaLocalOrigin(locals, idx))
        javaLocalsSuccessor((locals, idx)) = v
        v.declareDefault(rw)
    }
  }

  def initLocal(locals: JavaLocalDeclaration[Pre]): Statement[Post] = {
    implicit val o: Origin = locals.o
    Block(for((JavaVariableDeclaration(_, _, init), i) <- locals.decls.zipWithIndex if init.nonEmpty)
      yield assignLocal(Local(javaLocalsSuccessor.ref((locals, i))), rw.dispatch(init.get))
    )
  }

  def local(local: JavaLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o

    local.ref.get match {
      case RefAxiomaticDataType(decl) => throw NotAValue(local)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefUnloadedJavaNamespace(names) => throw NotAValue(local)
      case RefJavaClass(decl) => throw NotAValue(local)
      case RefJavaField(decls, idx) =>
        if(decls.modifiers.contains(JavaStatic[Pre]())) {
          val classStaticsFunction: LazyRef[Post, Function[Post]] = new LazyRef(javaStaticsFunctionSuccessor(javaClassDeclToJavaClass(decls)))
          Deref[Post](
            obj = FunctionInvocation[Post](classStaticsFunction, Nil, Nil, Nil, Nil)(PanicBlame("Statics singleton function requires nothing.")),
            ref = javaFieldsSuccessor.ref((decls, idx)),
          )(local.blame)
        } else {
          Deref[Post](rw.currentThis.top, javaFieldsSuccessor.ref((decls, idx)))(local.blame)
        }
      case RefModelField(field) =>
        ModelDeref[Post](rw.currentThis.top, rw.succ(field))(local.blame)
      case RefJavaLocalDeclaration(decls, idx) =>
        Local(javaLocalsSuccessor.ref((decls, idx)))
    }
  }

  def deref(deref: JavaDeref[Pre]): Expr[Post] = {
    implicit val o: Origin = deref.o

    deref.ref.get match {
      case RefAxiomaticDataType(decl) => throw NotAValue(deref)
      case RefModel(decl) => throw NotAValue(deref)
      case RefJavaClass(decl) => throw NotAValue(deref)
      case RefModelField(decl) => ModelDeref[Post](rw.dispatch(deref.obj), rw.succ(decl))(deref.blame)
      case RefUnloadedJavaNamespace(names) => throw NotAValue(deref)
      case RefJavaField(decls, idx) =>
        Deref[Post](rw.dispatch(deref.obj), javaFieldsSuccessor.ref((decls, idx)))(deref.blame)
      case BuiltinField(f) => rw.dispatch(f(deref.obj))
      case RefVariable(v) => ???
    }
  }

  def invocation(inv: JavaInvocation[Pre]): Expr[Post] = {
    val JavaInvocation(obj, typeParams, _, args, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefFunction(decl) =>
        FunctionInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case RefProcedure(decl) =>
        ProcedureInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil, typeParams.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case RefPredicate(decl) =>
        PredicateApply[Post](rw.succ(decl), args.map(rw.dispatch), WritePerm())
      case RefInstanceFunction(decl) =>
        InstanceFunctionInvocation[Post](obj.map(rw.dispatch).getOrElse(rw.currentThis.top), rw.succ(decl), args.map(rw.dispatch), typeParams.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case RefInstanceMethod(decl) =>
        MethodInvocation[Post](obj.map(rw.dispatch).getOrElse(rw.currentThis.top), rw.succ(decl), args.map(rw.dispatch), Nil, typeParams.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case RefInstancePredicate(decl) =>
        InstancePredicateApply[Post](obj.map(rw.dispatch).getOrElse(rw.currentThis.top), rw.succ(decl), args.map(rw.dispatch), WritePerm())
      case RefADTFunction(decl) =>
        ADTFunctionInvocation[Post](None, rw.succ(decl), args.map(rw.dispatch))
      case RefModelProcess(decl) =>
        ProcessApply[Post](rw.succ(decl), args.map(rw.dispatch))
      case RefModelAction(decl) =>
        ActionApply[Post](rw.succ(decl), args.map(rw.dispatch))
      case RefJavaMethod(decl) =>
        if(decl.modifiers.contains(JavaStatic[Pre]())) {
          val classStaticsFunction: LazyRef[Post, Function[Post]] = new LazyRef(javaStaticsFunctionSuccessor(javaClassDeclToJavaClass(decl)))
          MethodInvocation[Post](
            obj = FunctionInvocation[Post](classStaticsFunction, Nil, Nil, Nil, Nil)(inv.blame),
            ref = rw.succ(decl),
            args = args.map(rw.dispatch), outArgs = Nil, typeParams.map(rw.dispatch),
            givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) },
          )(inv.blame)
        } else {
          MethodInvocation[Post](
            obj = obj.map(rw.dispatch).getOrElse(rw.currentThis.top),
            ref = rw.succ(decl),
            args = args.map(rw.dispatch), outArgs = Nil, typeParams.map(rw.dispatch),
            givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
            yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) },
          )(inv.blame)
        }
      case RefJavaAnnotationMethod(decl) =>
        MethodInvocation[Post](
          obj = obj.map(rw.dispatch).getOrElse(rw.currentThis.top),
          ref = rw.succ(decl),
          args = Nil, outArgs = Nil, Nil, Nil, Nil
        )(inv.blame)
      case BuiltinInstanceMethod(f) =>
        rw.dispatch(f(obj.get)(args))
    }
  }

  def newClass(inv: JavaNewClass[Pre]): Expr[Post] = {
    val JavaNewClass(args, typeParams, t, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefModel(decl) => ModelNew[Post](rw.succ(decl))
      case RefJavaConstructor(cons) =>
        ProcedureInvocation[Post](rw.succ[Procedure[Post]](cons), args.map(rw.dispatch), Nil, typeParams.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case ImplicitDefaultJavaConstructor() =>
        val cls = t.asInstanceOf[JavaTClass[Pre]].ref.decl
        val succ = rw.lookupSuccessor
        val ref = new LazyRef[Post, Procedure[Post]](succ(javaDefaultConstructor(cls)).get)
        ProcedureInvocation[Post](ref,
          args.map(rw.dispatch), Nil, typeParams.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
    }
  }

  def newLiteralArray(arr: JavaNewLiteralArray[Pre]): Expr[Post] =
    rw.dispatch(arr.initializer)

  def newDefaultArray(arr: JavaNewDefaultArray[Pre]): Expr[Post] =
    NewArray(rw.dispatch(arr.baseType), arr.specifiedDims.map(rw.dispatch), arr.moreDims)(arr.o)

  def literalArray(arr: JavaLiteralArray[Pre]): Expr[Post] = {
    implicit val o: Origin = JavaInlineArrayInitializerOrigin(arr.o)
    val array = new Variable[Post](rw.dispatch(arr.typeContext.get))
    ScopedExpr[Post](Seq(array), With(Block(
      assignLocal(array.get, NewArray(rw.dispatch(arr.typeContext.get.element), Seq(const[Post](arr.exprs.size)), 0))
        +: arr.exprs.zipWithIndex.map {
          case (value, index) => Assign[Post](AmbiguousSubscript(array.get, const(index))(JavaArrayInitializerBlame), rw.dispatch(value))(
            PanicBlame("Assignment for an explicit array initializer cannot fail."))
        }
    ), array.get))
  }

  def classType(t: JavaTClass[Pre]): Type[Post] =
    TClass(javaInstanceClassSuccessor.ref(t.ref.decl))
}
