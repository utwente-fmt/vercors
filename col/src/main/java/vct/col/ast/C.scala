package vct.col.ast

import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, C, CDerefTarget, CInvocationTarget, CNameTarget, CTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefCDeclaration, RefCFunctionDefinition, RefCGlobalDeclaration, RefCParam, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}
import vct.result.VerificationResult
import vct.result.VerificationResult.UserError

import scala.annotation.tailrec

sealed trait CDeclarationSpecifier extends NodeFamily

sealed trait CSpecificationModifier extends CDeclarationSpecifier
case class CPure()(implicit val o: Origin) extends CSpecificationModifier
case class CInline()(implicit val o: Origin) extends CSpecificationModifier

sealed trait CStorageClassSpecifier extends CDeclarationSpecifier
case class CTypedef()(implicit val o: Origin) extends CStorageClassSpecifier
case class CExtern()(implicit val o: Origin) extends CStorageClassSpecifier
case class CStatic()(implicit val o: Origin) extends CStorageClassSpecifier

sealed trait CTypeSpecifier extends CDeclarationSpecifier
case class CVoid()(implicit val o: Origin) extends CTypeSpecifier
case class CChar()(implicit val o: Origin) extends CTypeSpecifier
case class CShort()(implicit val o: Origin) extends CTypeSpecifier
case class CInt()(implicit val o: Origin) extends CTypeSpecifier
case class CLong()(implicit val o: Origin) extends CTypeSpecifier
case class CFloat()(implicit val o: Origin) extends CTypeSpecifier
case class CDouble()(implicit val o: Origin) extends CTypeSpecifier
case class CSigned()(implicit val o: Origin) extends CTypeSpecifier
case class CUnsigned()(implicit val o: Origin) extends CTypeSpecifier
case class CBool()(implicit val o: Origin) extends CTypeSpecifier
case class CTypedefName(name: String)(implicit val o: Origin) extends CTypeSpecifier {
  var ref: Option[CTypeNameTarget] = None
}
case class CSpecificationType(t: Type)(implicit val o: Origin) extends CTypeSpecifier

case class CTypeQualifierDeclarationSpecifier(typeQual: CTypeQualifier)(implicit val o: Origin)
  extends CDeclarationSpecifier

sealed trait CTypeQualifier extends NodeFamily
case class CConst()(implicit val o: Origin) extends CTypeQualifier
case class CRestrict()(implicit val o: Origin) extends CTypeQualifier
case class CVolatile()(implicit val o: Origin) extends CTypeQualifier
case class CAtomic()(implicit val o: Origin) extends CTypeQualifier

sealed trait CFunctionSpecifier extends CDeclarationSpecifier
sealed trait CAlignmentSpecifier extends CDeclarationSpecifier

sealed trait CGpgpuKernelSpecifier extends CDeclarationSpecifier
case class CKernel()(implicit val o: Origin) extends CGpgpuKernelSpecifier

case class CPointer(qualifiers: Seq[CTypeQualifier])
                   (implicit val o: Origin) extends NodeFamily

class CParam(val specifiers: Seq[CDeclarationSpecifier], val declarator: CDeclarator)(implicit val o: Origin)
  extends ExtraDeclarationKind {
  override def declareDefault(scope: ScopeContext): Unit = scope.cParams.top += this
}

sealed trait CDeclarator extends NodeFamily
case class CPointerDeclarator(pointers: Seq[CPointer], inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator
case class CArrayDeclarator(qualifiers: Seq[CTypeQualifier], size: Option[Expr], inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator
case class CTypedFunctionDeclarator(params: Seq[CParam], varargs: Boolean, inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator
case class CAnonymousFunctionDeclarator(params: Seq[String], inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator
case class CName(name: String)(implicit val o: Origin) extends CDeclarator

case class CInit(decl: CDeclarator, init: Option[Expr])(implicit val o: Origin)
  extends NodeFamily

class CDeclaration(val contract: ApplicableContract, val kernelInvariant: Expr,
                   val specs: Seq[CDeclarationSpecifier], val inits: Seq[CInit])(implicit val o: Origin)
  extends ExtraDeclarationKind {
  override def declareDefault(scope: ScopeContext): Unit = scope.cLocalScopes.top += this
  override def check(context: CheckContext): Seq[CheckError] = kernelInvariant.checkSubType(TResource())
}

sealed trait CAbstractGlobalDeclaration extends ExtraGlobalDeclaration

class CFunctionDefinition(val specs: Seq[CDeclarationSpecifier], val declarator: CDeclarator, val body: Statement)(implicit val o: Origin)
  extends CAbstractGlobalDeclaration

class CGlobalDeclaration(val decl: CDeclaration)(implicit val o: Origin)
  extends CAbstractGlobalDeclaration

sealed trait CStatement extends ExtraStatement
case class CDeclarationStatement(decl: CDeclaration)(implicit val o: Origin) extends CStatement
case class CLabeledStatement(label: LabelDecl, statement: Statement)(implicit val o: Origin) extends CStatement
case class CGoto(label: String)(implicit val o: Origin) extends CStatement {
  var ref: Option[LabelDecl] = None
}

case class GpgpuLocalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin) extends CStatement
case class GpgpuGlobalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin) extends CStatement
case class GpgpuAtomic(impl: Statement, before: Statement, after: Statement)(implicit val o: Origin) extends CStatement

sealed trait CExpr extends ExtraExpr
case class CLocal(name: String)(implicit val o: Origin) extends CExpr {
  var ref: Option[CNameTarget] = None
  override def t: Type = ref.get match {
    case ref: RefCParam => C.typeOrReturnTypeFromDeclaration(ref.decl.specifiers, ref.decl.declarator)
    case ref: RefAxiomaticDataType => TNotAValue(ref)
    case RefVariable(decl) => decl.t
    case ref: RefCFunctionDefinition => TNotAValue(ref)
    case ref @ RefCGlobalDeclaration(decls, initIdx) =>
      val declInfo = C.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => TNotAValue(ref) // Function declaration
        case None => declInfo.typeOrReturnType(CPrimitiveType(decls.decl.specs)) // Static declaration
      }
    case ref @ RefCDeclaration(decls, initIdx) =>
      val declInfo = C.getDeclaratorInfo(decls.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => TNotAValue(ref) // Function declaration
        case None => declInfo.typeOrReturnType(CPrimitiveType(decls.specs)) // Static declaration
      }
    case RefModelField(field) => field.t
  }
}
case class CInvocation(applicable: Expr, args: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])
                      (implicit val o: Origin) extends CExpr {
  var ref: Option[CInvocationTarget] = None
  override def t: Type = ref.get match {
    case RefFunction(decl) =>  decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefCFunctionDefinition(decl) => C.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) => C.typeOrReturnTypeFromDeclaration(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefCDeclaration(decls, initIdx) => C.typeOrReturnTypeFromDeclaration(decls.specs, decls.inits(initIdx).decl)
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case BuiltinInstanceMethod(f) => applicable match {
      case CStructAccess(obj, _) => f(obj)(args).t
    }
  }
}
case class CStructAccess(struct: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends CExpr {
  var ref: Option[CDerefTarget] = None
  override def t: Type = ref.get match {
    case ref: RefModelField => ref.decl.t
    case ref: RefFunction => TNotAValue(ref)
    case ref: RefProcedure => TNotAValue(ref)
    case ref: RefPredicate => TNotAValue(ref)
    case ref: RefInstanceFunction => TNotAValue(ref)
    case ref: RefInstanceMethod => TNotAValue(ref)
    case ref: RefInstancePredicate => TNotAValue(ref)
    case ref: RefADTFunction => TNotAValue(ref)
    case ref: RefModelProcess => TNotAValue(ref)
    case ref: RefModelAction => TNotAValue(ref)
    case ref: BuiltinField => ref.f(struct).t
    case ref: BuiltinInstanceMethod => TNotAValue(ref)
  }
}
case class CStructDeref(struct: Expr, field: String)(implicit val o: Origin) extends CExpr {
  override def t: Type = ???
}
case class GpgpuCudaKernelInvocation(kernel: String, blocks: Expr, threads: Expr, args: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(implicit val o: Origin) extends CExpr {
  var ref: Option[CInvocationTarget] = None
  override def t: Type = ref.get match {
    case RefFunction(decl) => decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefCFunctionDefinition(decl) => C.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) => C.typeOrReturnTypeFromDeclaration(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefCDeclaration(decls, initIdx) => C.typeOrReturnTypeFromDeclaration(decls.specs, decls.inits(initIdx).decl)
    case BuiltinInstanceMethod(f) => ???
  }
}

sealed trait CType extends ExtraType
case class CPrimitiveType(specifiers: Seq[CDeclarationSpecifier])(implicit val o: Origin = DiagnosticOrigin) extends CType