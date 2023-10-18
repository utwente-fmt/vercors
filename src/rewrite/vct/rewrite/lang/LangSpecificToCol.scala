package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.lang.LangBipToCol
import vct.col.origin._
import vct.col.resolve.ctx._
import vct.col.resolve.lang.Java
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

case object LangSpecificToCol extends RewriterBuilder {
  override def key: String = "langSpecific"
  override def desc: String =
    "Translate language-specific constructs to a common subset of nodes."

  case object ThisVar extends Origin {
    override def preferredName: String = "this"
    override def shortPosition: String = "generated"
    override def context: String =
      "[At node generated to store the this value for constructors]"
    override def inlineContext: String = "this"
  }

  case class NotAValue(value: Expr[_]) extends UserError {
    override def code: String = "notAValue"
    override def text: String =
      value.o.messageInContext("Could not resolve this expression to a value.")
  }
}

case class LangSpecificToCol[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  val java: LangJavaToCol[Pre] = LangJavaToCol(this)
  val bip: LangBipToCol[Pre] = LangBipToCol(this)
  val c: LangCToCol[Pre] = LangCToCol(this)
  val cpp: LangCPPToCol[Pre] = LangCPPToCol(this)
  val pvl: LangPVLToCol[Pre] = LangPVLToCol(this)
  val silver: LangSilverToCol[Pre] = LangSilverToCol(this)
  val llvm: LangLLVMToCol[Pre] = LangLLVMToCol(this)

  val currentThis: ScopedStack[Expr[Post]] = ScopedStack()
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case model: Model[Pre] =>
        implicit val o: Origin = model.o
        currentThis.having(ThisModel[Post](succ(model))) {
          globalDeclarations.succeed(model, model.rewrite())
        }
      case seqProg: VeyMontSeqProg[Pre] =>
        implicit val o: Origin = seqProg.o
        currentThis.having(ThisSeqProg[Post](succ(seqProg))) {
          globalDeclarations.succeed(seqProg, seqProg.rewrite())
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
      case ns: CPPNamespaceDefinition[Pre] => cpp.rewriteNamespaceDef(ns)
      case decl: CPPGlobalDeclaration[Pre] => cpp.rewriteGlobalDecl(decl)
      case decl: CPPLocalDeclaration[Pre] => ???

      case func: LlvmFunctionDefinition[Pre] => llvm.rewriteFunctionDef(func)
      case global: LlvmGlobal[Pre] => llvm.rewriteGlobal(global)

      case cls: Class[Pre] =>
        currentClass.having(cls) {
          currentThis.having(ThisObject[Post](succ(cls))(cls.o)) {
            val decls =
              classDeclarations.collect {
                cls.declarations.foreach(dispatch)
                pvl.maybeDeclareDefaultConstructor(cls)
              }._1

            globalDeclarations.succeed(cls, cls.rewrite(decls))
          }
        }

      case glue: JavaBipGlueContainer[Pre] => bip.rewriteGlue(glue)

      case other => rewriteDefault(other)
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case scope @ Scope(locals, body) =>
        def scanScope(node: Node[Pre]): Unit =
          node match {
            case Scope(_, _) =>
            case JavaLocalDeclarationStatement(
                  locals: JavaLocalDeclaration[Pre]
                ) =>
              java.declareLocal(locals)
            case other => other.subnodes.foreach(scanScope)
          }

        scope.rewrite(locals =
          variables.collect {
            locals.foreach(dispatch)
            scanScope(body)
          }._1
        )

      case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) =>
        java.initLocal(locals)

      case CDeclarationStatement(decl) => c.rewriteLocal(decl)
      case CPPDeclarationStatement(decl) => cpp.rewriteLocal(decl)
      case goto: CGoto[Pre] => c.rewriteGoto(goto)
      case barrier: GpgpuBarrier[Pre] => c.gpuBarrier(barrier)

      case other => rewriteDefault(other)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
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

      case diz @ AmbiguousThis() => currentThis.top

      case local: JavaLocal[Pre] => java.local(local)
      case deref: JavaDeref[Pre] => java.deref(deref)
      case inv: JavaInvocation[Pre] => java.invocation(inv)
      case inv: JavaNewClass[Pre] => java.newClass(inv)
      case arr: JavaNewLiteralArray[Pre] => java.newLiteralArray(arr)
      case arr: JavaNewDefaultArray[Pre] => java.newDefaultArray(arr)
      case str: JavaStringValue[Pre] => java.stringValue(str)
      case arr: JavaLiteralArray[Pre] => java.literalArray(arr)

      case Cast(inner, TypeValue(t))
          if t == Java.float[Pre] || t == Java.double[Pre] =>
        CastFloat(dispatch(inner), dispatch(t))(e.o)

      case local: PVLLocal[Pre] => pvl.local(local)
      case deref: PVLDeref[Pre] => pvl.deref(deref)
      case inv: PVLNew[Pre] => pvl.newClass(inv)
      case inv: PVLInvocation[Pre] => pvl.invocation(inv)

      case local: CLocal[Pre] => c.local(local)
      case deref: CStructAccess[Pre] => c.deref(deref)
      case inv: CInvocation[Pre] => c.invocation(inv)
      case shared: SharedMemSize[Pre] => c.sharedSize(shared)
      case kernel: GpgpuCudaKernelInvocation[Pre] =>
        c.cudaKernelInvocation(kernel)
      case local: LocalThreadId[Pre] => c.cudaLocalThreadId(local)
      case global: GlobalThreadId[Pre] => c.cudaGlobalThreadId(global)
      case cast: CCast[Pre] => c.cast(cast)

      case local: CPPLocal[Pre] => cpp.local(local)
      case inv: CPPInvocation[Pre] => cpp.invocation(inv)

      case inv: SilverPartialADTFunctionInvocation[Pre] =>
        silver.adtInvocation(inv)
      case map: SilverUntypedNonemptyLiteralMap[Pre] => silver.nonemptyMap(map)

      case inv: LlvmFunctionInvocation[Pre] =>
        llvm.rewriteFunctionInvocation(inv)
      case inv: LlvmAmbiguousFunctionInvocation[Pre] =>
        llvm.rewriteAmbiguousFunctionInvocation(inv)
      case local: LlvmLocal[Pre] => llvm.rewriteLocal(local)

      case other => rewriteDefault(other)
    }

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case t: JavaTClass[Pre] => java.classType(t)
      case t: CTPointer[Pre] => c.pointerType(t)
      case t: CTArray[Pre] => c.arrayType(t)
      case t: CPPTArray[Pre] => cpp.arrayType(t)
      case other => rewriteDefault(other)
    }
}
