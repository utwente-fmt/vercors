package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.rewrite.lang.LangBipToCol
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx._
import vct.col.resolve.lang.Java
import vct.rewrite.lang.LangSpecificToCol.NotAValue
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg}
import vct.result.VerificationError.UserError
import vct.rewrite.lang.LangVeyMontToCol

case object LangSpecificToCol extends RewriterBuilderArg[Boolean] {
  override def key: String = "langSpecific"
  override def desc: String = "Translate language-specific constructs to a common subset of nodes."

  def ThisVar(): Origin = Origin(
    Seq(
      PreferredName(Seq("this")),
      LabelContext("constructor this"),
    )
  )

  case class NotAValue(value: Expr[_]) extends UserError {
    override def code: String = "notAValue"
    override def text: String = value.o.messageInContext("Could not resolve this expression to a value.")
  }
}

case class LangSpecificToCol[Pre <: Generation](veymontGeneratePermissions: Boolean = false) extends Rewriter[Pre] with LazyLogging {
  val java: LangJavaToCol[Pre] = LangJavaToCol(this)
  val bip: LangBipToCol[Pre] = LangBipToCol(this)
  val c: LangCToCol[Pre] = LangCToCol(this)
  val cpp: LangCPPToCol[Pre] = LangCPPToCol(this)
  val pvl: LangPVLToCol[Pre] = LangPVLToCol(this, veymontGeneratePermissions)
  val veymont: LangVeyMontToCol[Pre] = LangVeyMontToCol(this)
  val silver: LangSilverToCol[Pre] = LangSilverToCol(this)
  val llvm: LangLLVMToCol[Pre] = LangLLVMToCol(this)

  val currentThis: ScopedStack[Expr[Post]] = ScopedStack()
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  def specLocal(target: SpecNameTarget[Pre], e: Expr[Pre], blame: Blame[DerefInsufficientPermission]): Expr[Post] = target match {
    case RefAxiomaticDataType(_) => throw NotAValue(e)
    case RefClass(_) => throw NotAValue(e)
    case RefEnum(_) => throw NotAValue(e)
    case RefEnumConstant(enum, decl) => EnumUse[Post](succ(enum.get), succ(decl))(e.o)
    case RefVariable(decl) => Local[Post](succ(decl))(e.o)
    case RefModelField(decl) => ModelDeref[Post](currentThis.top, succ(decl))(blame)(e.o)
  }

  def specDeref(obj: Expr[Pre], target: SpecDerefTarget[Pre], e: Expr[Pre], blame: Blame[DerefInsufficientPermission]): Expr[Post] = target match {
    case RefEnumConstant(enum, decl) => EnumUse[Post](succ(enum.get), succ(decl))(e.o)
    case RefModelField(decl) => ModelDeref[Post](dispatch(obj), succ(decl))(blame)(e.o)
    case BuiltinField(f) => dispatch(f(obj))
  }

  def specInvocation(objPre: Option[Expr[Pre]],
                     target: SpecInvocationTarget[Pre],
                     typeArgs: Seq[Type[Pre]],
                     args: Seq[Expr[Pre]],
                     givenArgsPre: Seq[(Ref[Pre, Variable[Pre]], Expr[Pre])],
                     yieldsPre: Seq[(Expr[Pre], Ref[Pre, Variable[Pre]])],
                     e: Expr[Pre],
                     blame: Blame[FrontendInvocationError]
                    ): Expr[Post] = {
    implicit val o: Origin = e.o
    lazy val obj = objPre.map(dispatch).getOrElse(currentThis.top)
    lazy val givenArgs = givenArgsPre.map { case (Ref(v), e) => (succ[Variable[Post]](v), dispatch(e)) }
    lazy val yields = yieldsPre.map { case (e, Ref(v)) => (dispatch(e), succ[Variable[Post]](v)) }

    target match {
      case RefFunction(decl) => FunctionInvocation[Post](succ(decl), args.map(dispatch), typeArgs.map(dispatch), givenArgs, yields)(blame)
      case RefProcedure(decl) => ProcedureInvocation[Post](succ(decl), args.map(dispatch), Nil, typeArgs.map(dispatch), givenArgs, yields)(blame)
      case RefPredicate(decl) => PredicateApply[Post](succ(decl), args.map(dispatch), WritePerm())
      case RefADTFunction(decl) => ADTFunctionInvocation(None, succ(decl), args.map(dispatch))
      case RefProverFunction(decl) => ProverFunctionInvocation(succ(decl), args.map(dispatch))

      case RefInstanceFunction(decl) => InstanceFunctionInvocation[Post](obj, succ(decl), args.map(dispatch), typeArgs.map(dispatch), givenArgs, yields)(blame)
      case RefInstanceMethod(decl) => MethodInvocation[Post](obj, succ(decl), args.map(dispatch), Nil, typeArgs.map(dispatch), givenArgs, yields)(blame)
      case RefInstancePredicate(decl) => InstancePredicateApply[Post](obj, succ(decl), args.map(dispatch), WritePerm())

      case RefModelProcess(decl) => ProcessApply[Post](succ(decl), args.map(dispatch))
      case RefModelAction(decl) => ActionApply[Post](succ(decl), args.map(dispatch))

      case BuiltinInstanceMethod(f) => dispatch(f(objPre.get)(args))
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case model: Model[Pre] =>
      implicit val o: Origin = model.o
      currentThis.having(ThisModel[Post](succ(model))) {
        globalDeclarations.succeed(model, model.rewrite())
      }

    case ns: JavaNamespace[Pre] => java.rewriteNamespace(ns)
    case cls: JavaClassOrInterface[Pre] => java.rewriteClass(cls)
    case p: JavaParam[Pre] => java.rewriteParameter(p)

    case cons: PVLConstructor[Pre] => pvl.rewriteConstructor(cons)

    case method: JavaMethod[Pre] => java.rewriteMethod(method)

    case unit: CTranslationUnit[Pre] => c.rewriteUnit(unit)
    case cParam: CParam[Pre] => c.rewriteParam(cParam)
    case func: CFunctionDefinition[Pre] => c.rewriteFunctionDef(func)
    case decl: CGlobalDeclaration[Pre] => c.rewriteGlobalDecl(decl)
    case decl: CLocalDeclaration[Pre] => ???

    case unit: CPPTranslationUnit[Pre] => cpp.rewriteUnit(unit)
    case cppParam: CPPParam[Pre] => cpp.rewriteParam(cppParam)
    case func: CPPFunctionDefinition[Pre] => cpp.rewriteFunctionDef(func)
    case decl: CPPGlobalDeclaration[Pre] =>
      cpp.rewriteGlobalDecl(decl)
    case decl: CPPLocalDeclaration[Pre] => ???
    case pred: Predicate[Pre] => {
      cpp.storePredicate(pred)
      rewriteDefault(pred)
    }

    case func: LlvmFunctionDefinition[Pre] => llvm.rewriteFunctionDef(func)
    case global: LlvmGlobal[Pre] => llvm.rewriteGlobal(global)

    case cls: Class[Pre] =>
      currentClass.having(cls) {
        currentThis.having(ThisObject[Post](succ(cls))(cls.o)) {
          val decls = classDeclarations.collect {
            cls.declarations.foreach(dispatch)
            pvl.maybeDeclareDefaultConstructor(cls)
          }._1

          globalDeclarations.succeed(cls, cls.rewrite(decls))
        }
      }

    case glue: JavaBipGlueContainer[Pre] => bip.rewriteGlue(glue)

    case seqProg: PVLSeqProg[Pre] => veymont.rewriteSeqProg(seqProg)

    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case scope @ Scope(locals, body) =>
      def scanScope(node: Node[Pre]): Unit = node match {
        case Scope(_, _) =>
        case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) => java.declareLocal(locals)
        case other => other.subnodes.foreach(scanScope)
      }

      scope.rewrite(locals = variables.collect {
        locals.foreach(dispatch)
        scanScope(body)
      }._1)

    case branch: PVLBranch[Pre] => pvl.branch(branch)
    case loop: PVLLoop[Pre] => pvl.loop(loop)

    case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) => java.initLocal(locals)

    case CDeclarationStatement(decl) => c.rewriteLocal(decl)
    case CPPDeclarationStatement(decl) => cpp.rewriteLocalDecl(decl)
    case scope: CPPLifetimeScope[Pre] => cpp.rewriteLifetimeScope(scope)
    case goto: CGoto[Pre] => c.rewriteGoto(goto)
    case barrier: GpgpuBarrier[Pre] => c.gpuBarrier(barrier)

    case eval@Eval(CPPInvocation(_, _, _, _)) => cpp.invocationStatement(eval)

    case communicate: PVLCommunicate[Pre] => veymont.rewriteCommunicate(communicate)
    case assign: PVLSeqAssign[Pre] => veymont.rewriteParAssign(assign)

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case result @ AmbiguousResult() =>
      implicit val o: Origin = result.o
      result.ref.get match {
        case ref: RefCFunctionDefinition[Pre] => c.result(ref)
        case ref: RefCGlobalDeclaration[Pre] => c.result(ref)
        case ref: RefCPPFunctionDefinition[Pre] => cpp.result(ref)
        case ref: RefCPPGlobalDeclaration[Pre] => cpp.result(ref)
        case ref: RefLlvmFunctionDefinition[Pre] => llvm.result(ref)
        case RefFunction(decl) => Result[Post](anySucc(decl))
        case RefProcedure(decl) => Result[Post](anySucc(decl))
        case RefJavaMethod(decl) => Result[Post](java.javaMethod.ref(decl))
        case RefJavaAnnotationMethod(decL) => ???
        case RefInstanceFunction(decl) => Result[Post](anySucc(decl))
        case RefInstanceMethod(decl) => Result[Post](anySucc(decl))
        case RefInstanceOperatorFunction(decl) => Result[Post](anySucc(decl))
        case RefInstanceOperatorMethod(decl) => Result[Post](anySucc(decl))
        case RefLlvmSpecFunction(decl) => Result[Post](anySucc(decl))
      }

    case diz @ AmbiguousThis() =>
      currentThis.top

    case local: JavaLocal[Pre] => java.local(local)
    case deref: JavaDeref[Pre] => java.deref(deref)
    case inv: JavaInvocation[Pre] => java.invocation(inv)
    case inv: JavaNewClass[Pre] => java.newClass(inv)
    case arr: JavaNewLiteralArray[Pre] => java.newLiteralArray(arr)
    case arr: JavaNewDefaultArray[Pre] => java.newDefaultArray(arr)
    case str: JavaStringValue[Pre] => java.stringValue(str)
    case arr: JavaLiteralArray[Pre] => java.literalArray(arr)

    case Cast(inner, TypeValue(t)) if t == Java.float[Pre] || t == Java.double[Pre] =>
      CastFloat(dispatch(inner), dispatch(t))(e.o)

    case local: PVLLocal[Pre] => pvl.local(local)
    case deref: PVLDeref[Pre] => pvl.deref(deref)
    case inv: PVLNew[Pre] => pvl.newClass(inv)
    case inv: PVLInvocation[Pre] => pvl.invocation(inv)

    case local: CLocal[Pre] => c.local(local)
    case deref: CStructAccess[Pre] => c.deref(deref)
    case inv: CInvocation[Pre] => c.invocation(inv)
    case shared: SharedMemSize[Pre] => c.sharedSize(shared)
    case kernel: GpgpuCudaKernelInvocation[Pre] => c.cudaKernelInvocation(kernel)
    case local: LocalThreadId[Pre] => c.cudaLocalThreadId(local)
    case global: GlobalThreadId[Pre] => c.cudaGlobalThreadId(global)
    case cast: CCast[Pre] => c.cast(cast)

    case local: CPPLocal[Pre] => cpp.local(local)
    case deref: CPPClassMethodOrFieldAccess[Pre] => cpp.deref(deref)
    case inv: CPPInvocation[Pre] => cpp.invocation(inv)
    case preAssign@PreAssignExpression(local@CPPLocal(_, _), _) => cpp.preAssignExpr(preAssign, local)
    case _: CPPLambdaDefinition[Pre] => ???
    case arrSub@AmbiguousSubscript(_, _) => cpp.rewriteAccessorSubscript(arrSub)

    case inv: SilverPartialADTFunctionInvocation[Pre] => silver.adtInvocation(inv)
    case map: SilverUntypedNonemptyLiteralMap[Pre] => silver.nonemptyMap(map)

    case inv: LlvmFunctionInvocation[Pre] => llvm.rewriteFunctionInvocation(inv)
    case inv: LlvmAmbiguousFunctionInvocation[Pre] => llvm.rewriteAmbiguousFunctionInvocation(inv)
    case local: LlvmLocal[Pre] => llvm.rewriteLocal(local)

    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case t: JavaTClass[Pre] => java.classType(t)
    case t: CTPointer[Pre] => c.pointerType(t)
    case t: CTArray[Pre] => c.arrayType(t)
    case t: CPPTArray[Pre] => cpp.arrayType(t)
    case other => rewriteDefault(other)
  }
}
