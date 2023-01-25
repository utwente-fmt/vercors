package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.resolve.ctx._
import vct.col.resolve.lang.Java
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError
import vct.col.util.SuccessionMap

case object LangSpecificToCol extends RewriterBuilder {
  override def key: String = "langSpecific"
  override def desc: String = "Translate language-specific constructs to a common subset of nodes."

  case object ThisVar extends Origin {
    override def preferredName: String = "this"
    override def shortPosition: String = "generated"
    override def context: String = "[At node generated to store the this value for constructors]"
    override def inlineContext: String = "this"
  }

  case class NotAValue(value: Expr[_]) extends UserError {
    override def code: String = "notAValue"
    override def text: String = value.o.messageInContext("Could not resolve this expression to a value.")
  }
}

case class LangSpecificToCol[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  val java: LangJavaToCol[Pre] = LangJavaToCol(this)
  val c: LangCToCol[Pre] = LangCToCol(this)
  val pvl: LangPVLToCol[Pre] = LangPVLToCol(this)
  val silver: LangSilverToCol[Pre] = LangSilverToCol(this)

  val currentThis: ScopedStack[Expr[Post]] = ScopedStack()
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  var concatStrings: Option[Function[Pre]] = None
  var internToString: Option[Function[Pre]] = None

  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.transSubnodes.foreach {
      case ns: JavaNamespace[Pre] =>
        if (ns.pkg.exists(_.names == Java.JAVA_LANG)) {
          ns.transSubnodes.foreach {
            case f: Function[Pre] =>
              f.o match {
                case SourceNameOrigin("concatStrings", _) => concatStrings = Some(f)
                case SourceNameOrigin("internToString", _) => internToString = Some(f)
                case _ =>
              }
            case _ =>
          }
        }

      case _ =>
    }

    super.dispatch(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case model: Model[Pre] =>
      implicit val o: Origin = model.o
      currentThis.having(ThisModel[Post](succ(model))) {
        globalDeclarations.succeed(model, model.rewrite())
      }

    case ns: JavaNamespace[Pre] => java.rewriteNamespace(ns)
    case cls: JavaClassOrInterface[Pre] => java.rewriteClass(cls)
    case cons: PVLConstructor[Pre] => pvl.rewriteConstructor(cons)

    case unit: CTranslationUnit[Pre] => c.rewriteUnit(unit)
    case cParam: CParam[Pre] => c.rewriteParam(cParam)
    case func: CFunctionDefinition[Pre] => c.rewriteFunctionDef(func)
    case decl: CGlobalDeclaration[Pre] => c.rewriteGlobalDecl(decl)
    case decl: CLocalDeclaration[Pre] => ???

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

    case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) => java.initLocal(locals)

    case CDeclarationStatement(decl) => c.rewriteLocal(decl)
    case goto: CGoto[Pre] => c.rewriteGoto(goto)
    case barrier: GpgpuBarrier[Pre] => c.gpuBarrier(barrier)

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case result @ AmbiguousResult() =>
      implicit val o: Origin = result.o
      result.ref.get match {
        case ref: RefCFunctionDefinition[Pre] => c.result(ref)
        case ref: RefCGlobalDeclaration[Pre] => c.result(ref)
        case RefFunction(decl) => Result[Post](anySucc(decl))
        case RefProcedure(decl) => Result[Post](anySucc(decl))
        case RefJavaMethod(decl) => Result[Post](java.javaMethod.ref(decl))
        case RefJavaAnnotationMethod(decL) => ???
        case RefInstanceFunction(decl) => Result[Post](anySucc(decl))
        case RefInstanceMethod(decl) => Result[Post](anySucc(decl))
      }

    case diz @ AmbiguousThis() =>
      currentThis.top

    case local: JavaLocal[Pre] => java.local(local)
    case deref: JavaDeref[Pre] => java.deref(deref)
    case inv: JavaInvocation[Pre] => java.invocation(inv)
    case inv: JavaNewClass[Pre] => java.newClass(inv)
    case arr: JavaNewLiteralArray[Pre] => java.newLiteralArray(arr)
    case arr: JavaNewDefaultArray[Pre] => java.newDefaultArray(arr)
    case str: JavaStringLiteral[Pre] => java.stringLiteral(str)
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

    case inv: SilverPartialADTFunctionInvocation[Pre] => silver.adtInvocation(inv)
    case map: SilverUntypedNonemptyLiteralMap[Pre] => silver.nonemptyMap(map)

    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case t: JavaTClass[Pre] => java.classType(t)
    case t: CTPointer[Pre] => c.pointerType(t)
    case t: CTArray[Pre] => c.arrayType(t)
    case other => rewriteDefault(other)
  }
}
