package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.{FuncTools, ScopedStack}
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.lang.java.JavaAnnotationEx
import vct.col.origin._
import vct.col.ref.{LazyRef, Ref}
import vct.col.resolve.ctx._
import vct.col.resolve.lang.JavaAnnotationData.{BipComponent, BipData}
import vct.col.resolve.lang.{Java, JavaAnnotationData}
import vct.rewrite.lang.LangSpecificToCol.{NotAValue, ThisVar}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

import scala.collection.mutable

case object LangJavaToCol {
  private def JavaFieldOrigin(fields: JavaFields[_], idx: Int): Origin =
    fields.decls(idx).o.where(name = fields.decls(idx).name)

  private def JavaLocalOrigin(
      locals: JavaLocalDeclaration[_],
      idx: Int,
  ): Origin = { locals.decls(idx).o.where(name = locals.decls(idx).name) }

  private def JavaConstructorOrigin(cons: JavaConstructor[_]): Origin = {
    cons.o.where(name = cons.name)
  }

  private def JavaMethodOrigin(method: JavaMethod[_]): Origin = {
    method.o.where(name = method.name)
  }

  private def JavaAnnotationMethodOrigin(
      method: JavaAnnotationMethod[_]
  ): Origin = { method.o.where(name = method.name) }

  private def JavaInstanceClassOrigin(cls: JavaClassOrInterface[_]): Origin = {
    cls.o.where(name = cls.name)
  }

  private def JavaStaticsClassOrigin(cls: JavaClassOrInterface[_]): Origin = {
    cls.o.where(name = cls.name + "Statics")
  }

  private def JavaStaticsClassSingletonOrigin(
      cls: JavaClassOrInterface[_]
  ): Origin = { cls.o.where(name = cls.name + "StaticsSingleton") }

  private def JavaInlineArrayInitializerOrigin(inner: Origin): Origin = {
    inner.where(name = "arrayInitializer")
  }

  case class InvalidArrayInitializerNesting(initializer: JavaLiteralArray[_])
      extends UserError {
    override def text: String =
      initializer.o.messageInContext(
        "This literal array is nested more deeply than its indicated type allows."
      )
    override def code: String = "invalidNesting"
  }

  case class NotSupportedInJavaLangStringClass(decl: ClassDeclaration[_])
      extends UserError {
    override def code: String =
      decl.o.messageInContext(
        "This declaration is not supported in the java.lang.String class"
      )
    override def text: String = "notSupportedInStringClass"
  }

  case class GenericJavaNotSupported(decl: JavaClassOrInterface[_])
      extends UserError {
    override def code: String =
      decl.o.messageInContext("Generic Java classes not supported")
    override def text: String = "genericJavaClass"
  }
}

case class LangJavaToCol[Pre <: Generation](rw: LangSpecificToCol[Pre])
    extends LazyLogging {
  import LangJavaToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val namespace: ScopedStack[JavaNamespace[Pre]] = ScopedStack()
  val javaInstanceClassSuccessor
      : SuccessionMap[JavaClassOrInterface[Pre], Class[Post]] = SuccessionMap()
  val javaStaticsClassSuccessor
      : SuccessionMap[JavaClassOrInterface[Pre], Class[Post]] = SuccessionMap()
  val javaStaticsFunctionSuccessor
      : SuccessionMap[JavaClassOrInterface[Pre], Function[Post]] =
    SuccessionMap()

  val javaFieldsSuccessor
      : SuccessionMap[(JavaFields[Pre], Int), InstanceField[Post]] =
    SuccessionMap()
  val javaLocalsSuccessor
      : SuccessionMap[(JavaLocalDeclaration[Pre], Int), Variable[Post]] =
    SuccessionMap()
  val javaParamSuccessor: SuccessionMap[JavaParam[Pre], Variable[Post]] =
    SuccessionMap()

  val javaMethod: SuccessionMap[JavaMethod[Pre], InstanceMethod[Post]] =
    SuccessionMap()
  val javaConstructor: SuccessionMap[JavaConstructor[Pre], Constructor[Post]] =
    SuccessionMap()
  val javaDefaultConstructor
      : SuccessionMap[JavaClassOrInterface[Pre], JavaConstructor[Pre]] =
    SuccessionMap()

  val javaClassDeclToJavaClass
      : mutable.Map[JavaClassDeclaration[Pre], JavaClassOrInterface[Pre]] =
    mutable.Map()

  val currentJavaClass: ScopedStack[JavaClassOrInterface[Pre]] = ScopedStack()

  def isJavaStatic(decl: ClassDeclaration[_]): Boolean =
    decl match {
      case init: JavaSharedInitialization[_] => init.isStatic
      case fields: JavaFields[_] =>
        fields.modifiers.collectFirst { case JavaStatic() => () }.nonEmpty
      case method: JavaMethod[_] =>
        method.modifiers.collectFirst { case JavaStatic() => () }.nonEmpty
      case _: JavaConstructor[_] => false
      case _: ClassDeclaration[_] =>
        false // FIXME we should have a way of translating static specification-type declarations
    }

  def makeJavaClass(
      prefName: String,
      decls: Seq[ClassDeclaration[Pre]],
      ref: Ref[Post, Class[Post]],
      isStaticPart: Boolean,
  )(implicit o: Origin): Unit = {
    // First, declare all the fields, so we can refer to them.
    decls.foreach {
      case fields: JavaFields[Pre] =>
        fields.drop()
        for (
          (JavaVariableDeclaration(_, dims, _), idx) <-
            fields.decls.zipWithIndex
        ) {
          javaFieldsSuccessor((fields, idx)) =
            new InstanceField(
              t = FuncTools
                .repeat(TArray[Post](_), dims, rw.dispatch(fields.t)),
              flags = fields.modifiers.collect { case JavaFinal() =>
                new Final[Post]()
              },
            )(JavaFieldOrigin(fields, idx))
          rw.classDeclarations.declare(javaFieldsSuccessor((fields, idx)))
        }
      case _ =>
    }

    // Each constructor performs in order:
    // 1. the inline initialization of all fields

    val fieldInit =
      (diz: Expr[Post]) =>
        Block[Post](decls.collect { case fields: JavaFields[Pre] =>
          Block(
            for (
              (JavaVariableDeclaration(_, dims, init), idx) <-
                fields.decls.zipWithIndex
            )
              yield init match {
                case Some(value) =>
                  assignField[Post](
                    obj = diz,
                    field = javaFieldsSuccessor.ref((fields, idx)),
                    value = rw.dispatch(value),
                    blame = PanicBlame(
                      "The inline initialization of a field must have permission, because it is the first initialization that happens."
                    ),
                  )
                case None if fields.modifiers.collectFirst { case JavaFinal() =>
                      ()
                    }.isEmpty =>
                  assignField[Post](
                    obj = diz,
                    field = javaFieldsSuccessor.ref((fields, idx)),
                    value = Java.zeroValue(
                      FuncTools
                        .repeat(TArray[Post](_), dims, rw.dispatch(fields.t))
                    ),
                    blame = PanicBlame(
                      "The inline initialization of a field must have permission, because it is the first initialization that happens."
                    ),
                  )
                case None /* if modifiers contains final */ => Block(Nil)
              }
          )
        })

    // 2. the shared initialization blocks

    val sharedInit =
      (diz: Expr[Post]) => {
        rw.currentThis.having(diz) {
          Block(decls.collect { case init: JavaSharedInitialization[Pre] =>
            rw.dispatch(init.initialization)
          })
        }
      }

    // 3. the body of the constructor

    val declsDefault =
      if (decls.collect { case _: JavaConstructor[Pre] => () }.isEmpty) {
        val fieldPerms: UnitAccountedPredicate[Pre] =
          if (BipComponent.get(currentJavaClass.top).isDefined) {
            // Permissions are managed by bip permission generation & the bip component invariant, so
            // don't generate permissions here
            UnitAccountedPredicate(tt)
          } else {
            UnitAccountedPredicate(foldStar(decls.collect {
              case fields: JavaFields[Pre] if fields.modifiers.collectFirst {
                    case JavaFinal() => ()
                  }.isEmpty =>
                fields.decls.indices.map(decl => {
                  val local = JavaLocal[Pre](fields.decls(decl).name)(DerefPerm)
                  local.ref = Some(RefJavaField[Pre](fields, decl))
                  Perm(
                    AmbiguousLocation(local)(PanicBlame(
                      "Field location is not a pointer."
                    )),
                    WritePerm(),
                  )
                })
            }.flatten))
          }

        val cons =
          new JavaConstructor[Pre](
            modifiers = Nil,
            name = prefName,
            parameters = Nil,
            typeParameters = Nil,
            signals = Nil,
            body = Block(Nil),
            contract =
              ApplicableContract[Pre](
                requires = UnitAccountedPredicate(tt),
                /* Hack: don't generate the permissions for the default constructor. Problem: the "local" method further down will use
             the Statics function instead of \result from the procedure. Statics constructor is only used for static final
             field assignments required for ConstantifyFinalFields.
                 */
                ensures =
                  if (isStaticPart)
                    UnitAccountedPredicate(tt[Pre])
                  else
                    fieldPerms,
                contextEverywhere = tt,
                signals = Nil,
                givenArgs = Nil,
                yieldsArgs = Nil,
                decreases = None,
              )(TrueSatisfiable),
          )(PanicBlame(
            "The postcondition of a default constructor cannot fail."
          ))
        if (!isStaticPart)
          javaDefaultConstructor(currentJavaClass.top) = cons
        cons +: decls
      } else
        decls

    declsDefault.foreach {
      case cons: JavaConstructor[Pre] =>
        logger.debug(s"Constructor for ${cons.o.inlineContextText}")
        implicit val o: Origin = cons.o
        val t = TClass(ref, Seq())
        val `this` = ThisObject(ref)

        val results = currentJavaClass.top.modifiers.collect {
          case annotation @ JavaAnnotationEx(
                _,
                _,
                component @ JavaAnnotationData.BipComponent(_, _),
              ) if !isStaticPart =>
            rw.bip.rewriteConstructor(
              cons,
              annotation,
              component,
              diz => Block[Post](Seq(fieldInit(diz), sharedInit(diz))),
            )
        }
        if (results.isEmpty) { // We didn't execute the bip rewrite, so we do the normal one
          rw.labelDecls.scope {
            javaConstructor(cons) = rw.classDeclarations.declare(
              new Constructor[Post](
                cls = ref,
                args =
                  rw.variables.collect { cons.parameters.map(rw.dispatch) }._1,
                outArgs = Nil,
                typeArgs = Nil,
                body = Some(rw.currentThis.having(`this`) {
                  Block(Seq(
                    fieldInit(`this`),
                    sharedInit(`this`),
                    rw.dispatch(cons.body),
                  ))
                }),
                contract =
                  rw.currentThis.having(`this`) {
                    cons.contract.rewrite(signals =
                      cons.contract.signals.map(rw.dispatch) ++
                        cons.signals.map(t =>
                          SignalsClause(new Variable(rw.dispatch(t)), tt)
                        )
                    )
                  },
              )(cons.blame)(JavaConstructorOrigin(cons))
            )
          }
        }
      case method: JavaMethod[Pre] =>
        // For each javabip annotation that we encounter, execute a rewrite
        val results = method.modifiers.collect {
          case annotation @ JavaAnnotationEx(
                _,
                _,
                guard: JavaAnnotationData.BipGuard[Pre],
              ) =>
            rw.bip.rewriteGuard(method, annotation, guard)
          case annotation @ JavaAnnotationEx(
                _,
                _,
                transition: JavaAnnotationData.BipTransition[Pre],
              ) =>
            rw.bip.rewriteTransition(method, annotation, transition)
          case annotation @ JavaAnnotationEx(
                _,
                _,
                data: JavaAnnotationData.BipData[Pre],
              ) =>
            rw.bip.rewriteOutgoingData(method, annotation, data)
        }
        // If no rewrites were triggered, it must be a regular java method, so execute the default rewrite
        if (results.isEmpty) { rw.dispatch(method) }

      case method: JavaAnnotationMethod[Pre] =>
        rw.classDeclarations.succeed(
          method,
          new InstanceMethod(
            returnType = rw.dispatch(method.returnType),
            args = Nil,
            outArgs = Nil,
            typeArgs = Nil,
            body = None,
            contract = contract(TrueSatisfiable),
          )(PanicBlame("Verification of annotation method cannot fail"))(
            JavaAnnotationMethodOrigin(method)
          ),
        )
      case _: JavaSharedInitialization[Pre] =>
      case _: JavaFields[Pre] =>
      case other => rw.dispatch(other)
    }
  }

  def rewriteMethod(method: JavaMethod[Pre]): Unit = {
    implicit val o: Origin = method.o
    rw.labelDecls.scope {
      javaMethod(method) = rw.classDeclarations.declare(
        new InstanceMethod(
          returnType = rw.dispatch(method.returnType),
          args = rw.variables.collect(method.parameters.map(rw.dispatch(_)))._1,
          outArgs = Nil,
          typeArgs = Nil,
          body =
            method.modifiers.collectFirst { case sync @ JavaSynchronized() =>
              sync
            } match {
              case Some(sync) =>
                method.body.map(body =>
                  Synchronized(rw.currentThis.top, rw.dispatch(body))(
                    sync.blame
                  )(method.o)
                )
              case None => method.body.map(rw.dispatch)
            },
          contract = method.contract.rewrite(signals =
            method.contract.signals.map(rw.dispatch) ++
              method.signals
                .map(t => SignalsClause(new Variable(rw.dispatch(t)), tt))
          ),
          doInline =
            method.modifiers.collectFirst { case JavaInline() => () }.nonEmpty,
          pure =
            method.modifiers.collectFirst { case JavaPure() => () }.nonEmpty,
        )(method.blame)(JavaMethodOrigin(method))
      )
    }
  }

  def rewriteParameter(param: JavaParam[Pre]): Unit =
    if (BipData.get(param).isDefined) { rw.bip.rewriteParameter(param) }
    else {
      javaParamSuccessor(param) = rw.variables
        .declare(new Variable(rw.dispatch(param.t))(
          param.o.where(name = param.name)
        ))
    }

  def rewriteClass(cls: JavaClassOrInterface[Pre]): Unit = {
    implicit val o: Origin = cls.o

    cls.decls.collect({ case decl: JavaClassDeclaration[Pre] =>
      javaClassDeclToJavaClass(decl) = cls
    })

    currentJavaClass.having(cls) {
      val instDecls = cls.decls.filter(!isJavaStatic(_))
      val staticDecls = cls.decls.filter(isJavaStatic)

      val lockInvariant =
        cls match {
          case clazz: JavaClass[Pre] => clazz.intrinsicLockInvariant
          case _: JavaInterface[Pre] => tt[Pre]
          case _: JavaAnnotationInterface[Pre] => tt[Pre]
        }

      val instanceClass =
        rw.currentThis.having(ThisObject(javaInstanceClassSuccessor.ref(cls))) {
          new Class[Post](
            rw.variables.dispatch(cls.typeParams)(rw),
            rw.classDeclarations.collect {
              makeJavaClass(
                cls.name,
                instDecls,
                javaInstanceClassSuccessor.ref(cls),
                isStaticPart = false,
              )
              cls match {
                case cls: JavaClass[Pre] if BipComponent.get(cls).isDefined =>
                  rw.bip.generateComponent(cls)
                case _ =>
              }
            }._1,
            cls.supports.map(rw.dispatch),
            rw.dispatch(lockInvariant),
          )(JavaInstanceClassOrigin(cls))
        }

      rw.globalDeclarations.declare(instanceClass)
      javaInstanceClassSuccessor(cls) = instanceClass

      if (staticDecls.nonEmpty) {
        val staticsClass =
          new Class[Post](
            Seq(),
            rw.classDeclarations.collect {
              rw.currentThis
                .having(ThisObject(javaStaticsClassSuccessor.ref(cls))) {
                  makeJavaClass(
                    cls.name + "Statics",
                    staticDecls,
                    javaStaticsClassSuccessor.ref(cls),
                    isStaticPart = true,
                  )
                }
            }._1,
            Nil,
            tt,
          )(JavaStaticsClassOrigin(cls))

        rw.globalDeclarations.declare(staticsClass)
        val t = TClass[Post](staticsClass.ref, Seq())
        val singleton = withResult((res: Result[Post]) =>
          function(
            AbstractApplicable,
            TrueSatisfiable,
            returnType = t,
            ensures = UnitAccountedPredicate(
              (res !== Null()) && (TypeOf(res) === TypeValue(t))
            ),
          )(JavaStaticsClassSingletonOrigin(cls))
        )
        rw.globalDeclarations.declare(singleton)
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
        val v =
          new Variable[Post](
            FuncTools.repeat(TArray[Post](_), dims, rw.dispatch(locals.t))
          )(JavaLocalOrigin(locals, idx))
        javaLocalsSuccessor((locals, idx)) = v
        rw.variables.declare(v)
    }
  }

  def initLocal(locals: JavaLocalDeclaration[Pre]): Statement[Post] = {
    implicit val o: Origin = locals.o
    Block(
      for (
        (JavaVariableDeclaration(_, dims, init), i) <- locals.decls.zipWithIndex
      )
        yield assignLocal(
          Local(javaLocalsSuccessor.ref((locals, i))),
          init match {
            case Some(value) => rw.dispatch(value)
            case None =>
              Java.zeroValue(
                FuncTools.repeat(TArray[Post](_), dims, rw.dispatch(locals.t))
              )
          },
        )
    )
  }

  /** Provides the singleton object needed to access static fields/methods of a
    * class.
    * @param cls
    *   \- class for which we get the static singleton (lazy, because the class
    *   may not yet be known)
    * @return
    *   a singleton object to access static class fields/methods
    */
  def statics(
      cls: => JavaClassOrInterface[Pre]
  )(implicit o: Origin): Expr[Post] = {
    val classStaticsFunction: LazyRef[Post, Function[Post]] =
      new LazyRef(javaStaticsFunctionSuccessor(cls))
    FunctionInvocation[Post](classStaticsFunction, Nil, Nil, Nil, Nil)(
      PanicBlame("Statics singleton function requires nothing.")
    )
  }

  def local(local: JavaLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o

    local.ref.get match {
      case spec: SpecNameTarget[Pre] => rw.specLocal(spec, local, local.blame)
      case RefJavaParam(decl) if BipData.get(decl).isDefined =>
        rw.bip.local(local, decl)
      case RefJavaParam(decl) => Local(javaParamSuccessor.ref(decl))
      case RefUnloadedJavaNamespace(names) => throw NotAValue(local)
      case RefJavaClass(decl) => throw NotAValue(local)
      case RefJavaField(decls, idx) =>
        if (decls.modifiers.contains(JavaStatic[Pre]())) {
          Deref[Post](
            obj = statics(javaClassDeclToJavaClass(decls)),
            ref = javaFieldsSuccessor.ref((decls, idx)),
          )(local.blame)
        } else {
          Deref[Post](
            rw.currentThis.top,
            javaFieldsSuccessor.ref((decls, idx)),
          )(local.blame)
        }
      case RefJavaBipGuard(_) => rw.bip.local(local)
      case RefJavaLocalDeclaration(decls, idx) =>
        Local(javaLocalsSuccessor.ref((decls, idx)))
    }
  }

  def deref(deref: JavaDeref[Pre]): Expr[Post] = {
    implicit val o: Origin = deref.o

    deref.ref.get match {
      case spec: SpecDerefTarget[Pre] =>
        rw.specDeref(deref.obj, spec, deref, deref.blame)
      case _: SpecTypeNameTarget[Pre] => throw NotAValue(deref)
      case RefJavaClass(decl) => throw NotAValue(deref)
      case RefUnloadedJavaNamespace(names) => throw NotAValue(deref)
      case RefJavaField(decls, idx) =>
        if (decls.modifiers.contains(JavaStatic[Pre]())) {
          Deref[Post](
            obj = statics(javaClassDeclToJavaClass(decls)),
            ref = javaFieldsSuccessor.ref((decls, idx)),
          )(deref.blame)
        } else {
          Deref[Post](
            rw.dispatch(deref.obj),
            javaFieldsSuccessor.ref((decls, idx)),
          )(deref.blame)
        }
    }
  }

  def invocation(inv: JavaInvocation[Pre]): Expr[Post] = {
    val JavaInvocation(obj, typeParams, _, args, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case spec: SpecInvocationTarget[Pre] =>
        rw.specInvocation(
          inv.obj,
          spec,
          inv.typeParams,
          args,
          givenMap,
          yields,
          inv,
          inv.blame,
        )
      case RefJavaMethod(decl) =>
        if (decl.modifiers.contains(JavaStatic[Pre]())) {
          MethodInvocation[Post](
            obj = statics(javaClassDeclToJavaClass(decl)),
            ref = javaMethod.ref(decl),
            args = args.map(rw.dispatch),
            outArgs = Nil,
            typeParams.map(rw.dispatch),
            givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
            yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
          )(inv.blame)
        } else {
          MethodInvocation[Post](
            obj = obj.map(rw.dispatch).getOrElse(rw.currentThis.top),
            ref = javaMethod.ref(decl),
            args = args.map(rw.dispatch),
            outArgs = Nil,
            typeParams.map(rw.dispatch),
            givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
            yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
          )(inv.blame)
        }
      case RefJavaAnnotationMethod(decl) =>
        MethodInvocation[Post](
          obj = obj.map(rw.dispatch).getOrElse(rw.currentThis.top),
          ref = rw.succ(decl),
          args = Nil,
          outArgs = Nil,
          Nil,
          Nil,
          Nil,
        )(inv.blame)
    }
  }

  def newClass(inv: JavaNewClass[Pre]): Expr[Post] = {
    val JavaNewClass(args, typeParams, t, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefModel(decl) => ModelNew[Post](rw.succ(decl))
      case RefJavaConstructor(cons) =>
        ConstructorInvocation[Post](
          javaConstructor.ref(cons),
          Seq(),
          args.map(rw.dispatch),
          Nil,
          typeParams.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        )(inv.blame)
      case ImplicitDefaultJavaConstructor(_) =>
        val cls = t.asInstanceOf[JavaTClass[Pre]].ref.decl
        val ref =
          new LazyRef[Post, Constructor[Post]](javaConstructor(
            javaDefaultConstructor(cls)
          ))
        ConstructorInvocation[Post](
          ref,
          Seq(),
          args.map(rw.dispatch),
          Nil,
          typeParams.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        )(inv.blame)
    }
  }

  def newLiteralArray(arr: JavaNewLiteralArray[Pre]): Expr[Post] =
    rw.dispatch(arr.initializer)

  def newDefaultArray(arr: JavaNewDefaultArray[Pre]): Expr[Post] =
    NewArray(
      rw.dispatch(arr.baseType),
      arr.specifiedDims.map(rw.dispatch),
      arr.moreDims,
      true,
    )(arr.blame)(arr.o)

  def literalArray(arr: JavaLiteralArray[Pre]): Expr[Post] = {
    implicit val o: Origin = JavaInlineArrayInitializerOrigin(arr.o)
    val array = new Variable[Post](rw.dispatch(arr.typeContext.get))
    ScopedExpr[Post](
      Seq(array),
      With(
        Block(
          assignLocal(
            array.get,
            NewArray(
              rw.dispatch(arr.typeContext.get.element),
              Seq(const[Post](arr.exprs.size)),
              0,
              true,
            )(PanicBlame(
              "Assignment for an explicit array initializer cannot fail."
            )),
          ) +: arr.exprs.zipWithIndex.map { case (value, index) =>
            Assign[Post](
              AmbiguousSubscript(array.get, const(index))(
                JavaArrayInitializerBlame
              ),
              rw.dispatch(value),
            )(PanicBlame(
              "Assignment for an explicit array initializer cannot fail."
            ))
          }
        ),
        array.get,
      ),
    )
  }

  def stringValue(str: JavaStringValue[Pre]): Expr[Post] = {
    val JavaTClass(Ref(stringClass), Seq()) = str.t
    val intern: JavaMethod[Pre] =
      stringClass.declarations.collectFirst {
        case m: JavaMethod[Pre] if m.name == "vercorsIntern" => m
      }.get

    val classStaticsFunction: LazyRef[Post, Function[Post]] =
      new LazyRef(javaStaticsFunctionSuccessor(stringClass))
    MethodInvocation[Post](
      obj =
        FunctionInvocation[Post](classStaticsFunction, Nil, Nil, Nil, Nil)(
          PanicBlame("Class static cannot fail")
        )(str.o),
      ref = javaMethod.ref(intern),
      args = Seq(StringValue(str.data)(str.o)),
      Nil,
      Nil,
      Nil,
      Nil,
    )(PanicBlame("Interning cannot fail"))(str.o)
  }

  def classType(t: JavaTClass[Pre]): Type[Post] =
    t.ref.decl match {
      case classOrInterface: JavaClassOrInterface[Pre] =>
        TClass(
          javaInstanceClassSuccessor.ref(classOrInterface),
          t.typeArgs.map(rw.dispatch),
        )
    }
}
