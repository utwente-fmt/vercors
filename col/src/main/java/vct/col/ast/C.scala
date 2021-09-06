package vct.col.ast

import vct.col.ast.CPrimitiveType.NUMBER_LIKE_SPECIFIERS

import scala.annotation.tailrec

sealed trait CDeclarationSpecifier extends NodeFamily with NoCheck

sealed trait CSpecificationModifier extends CDeclarationSpecifier
case class CPure()(implicit val o: Origin) extends CSpecificationModifier
case class CInline()(implicit val o: Origin) extends CSpecificationModifier

sealed trait CStorageClassSpecifier extends CDeclarationSpecifier
case class CTypedef()(implicit val o: Origin) extends CStorageClassSpecifier
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
  var ref: Option[CRef] = None
}
case class CSpecificationType(t: Type)(implicit val o: Origin) extends CTypeSpecifier

case class CTypeQualifierDeclarationSpecifier(typeQual: CTypeQualifier)(implicit val o: Origin)
  extends CDeclarationSpecifier

sealed trait CTypeQualifier extends NodeFamily
case class CConst()(implicit val o: Origin) extends CTypeQualifier with NoCheck
case class CRestrict()(implicit val o: Origin) extends CTypeQualifier with NoCheck
case class CVolatile()(implicit val o: Origin) extends CTypeQualifier with NoCheck
case class CAtomic()(implicit val o: Origin) extends CTypeQualifier with NoCheck

sealed trait CFunctionSpecifier extends CDeclarationSpecifier
sealed trait CAlignmentSpecifier extends CDeclarationSpecifier

sealed trait CGpgpuKernelSpecifier extends CDeclarationSpecifier
case class CKernel()(implicit val o: Origin) extends CGpgpuKernelSpecifier

case class CPointer(qualifiers: Seq[CTypeQualifier])
                   (implicit val o: Origin) extends NodeFamily with NoCheck

case class CParam(specifiers: Seq[CDeclarationSpecifier], declarator: CDeclarator)(implicit val o: Origin)
  extends NodeFamily with NoCheck

sealed trait CDeclarator extends NodeFamily
case class CPointerDeclarator(pointers: Seq[CPointer], inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator with NoCheck
case class CArrayDeclarator(qualifiers: Seq[CTypeQualifier], size: Option[Expr], inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator with NoCheck
case class CTypedFunctionDeclarator(params: Seq[CParam], varargs: Boolean, inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator with NoCheck
case class CAnonymousFunctionDeclarator(params: Seq[String], inner: CDeclarator)(implicit val o: Origin)
  extends CDeclarator with NoCheck
case class CName(name: String)(implicit val o: Origin) extends CDeclarator with NoCheck

case class CInit(decl: CDeclarator, init: Option[Expr])(implicit val o: Origin)
  extends NodeFamily with NoCheck

case class CDeclaration(contract: ApplicableContract, kernelInvariant: Expr,
                        specs: Seq[CDeclarationSpecifier], inits: Seq[CInit])(implicit val o: Origin)
  extends Check(kernelInvariant.checkSubType(TResource())) with NodeFamily

sealed trait CAbstractGlobalDeclaration extends ExtraGlobalDeclaration

case class CFunctionDefinition(specs: Seq[CDeclarationSpecifier], declarator: CDeclarator, body: Statement)(implicit val o: Origin)
  extends CAbstractGlobalDeclaration with NoCheck

case class CGlobalDeclaration(decl: CDeclaration)(implicit val o: Origin)
  extends CAbstractGlobalDeclaration with NoCheck

sealed trait CStatement extends ExtraStatement
// TODO nothing in the tree of nodes under CDeclarationStatement is actually a Declaration, what to do?
case class CDeclarationStatement(decl: CDeclaration)(implicit val o: Origin) extends CStatement with NoCheck
case class CLabeledStatement(label: String, statement: Statement)(implicit val o: Origin) extends CStatement with NoCheck
case class CGoto(label: String)(implicit val o: Origin) extends CStatement with NoCheck {
  var ref: Option[CLabeledStatement] = None
}

case class GpgpuLocalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin)
  extends Check(requires.checkSubType(TResource()), ensures.checkSubType(TResource())) with CStatement
case class GpgpuGlobalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin)
  extends Check(requires.checkSubType(TResource()), ensures.checkSubType(TResource())) with CStatement
case class GpgpuAtomic(impl: Statement, before: Statement, after: Statement)(implicit val o: Origin) extends CStatement with NoCheck

sealed trait CExpr extends ExtraExpr
case class CLocal(name: String)(implicit val o: Origin) extends CExpr with NoCheck {
  var ref: Option[CRef] = None
  override def t: Type = ref.get.t
}
case class CInvocation(applicable: Expr, args: Seq[Expr])(implicit val o: Origin) extends CExpr with NoCheck {
  var ref: Option[CRef] = None
  override def t: Type = ref.get.t
}
case class CStructAccess(struct: Expr, field: String)(implicit val o: Origin) extends CExpr with NoCheck {
  override def t: Type = ???
}
case class CStructDeref(struct: Expr, field: String)(implicit val o: Origin) extends CExpr with NoCheck {
  override def t: Type = ???
}
case class GpgpuCudaKernelInvocation(kernel: String, blocks: Expr, threads: Expr, args: Seq[Expr])(implicit val o: Origin) extends CExpr with NoCheck {
  var ref: Option[CRef] = None
  override def t: Type = ref.get.t
}

sealed trait CType extends ExtraType

object CPrimitiveType {
  private implicit val o: Origin = DiagnosticOrigin

  val NUMBER_LIKE_PREFIXES: Seq[Seq[CDeclarationSpecifier]] = Seq(
    Nil,
    Seq(CUnsigned()),
    Seq(CSigned()),
  )

  val NUMBER_LIKE_TYPES: Seq[Seq[CDeclarationSpecifier]] = Seq(
    Seq(CInt()),
    Seq(CLong()),
    Seq(CLong(), CInt()),
    Seq(CLong(), CLong()),
    Seq(CLong(), CLong(), CInt()),
  )

  val NUMBER_LIKE_SPECIFIERS: Seq[Seq[CDeclarationSpecifier]] =
    for (prefix <- NUMBER_LIKE_PREFIXES; t <- NUMBER_LIKE_TYPES)
      yield prefix ++ t

  @tailrec
  private def foldDeclarator(t: Type, declarator: CDeclarator): Type = declarator match {
    case CPointerDeclarator(pointers, inner) =>
      foldDeclarator(
        pointers.foldLeft(t)((t, _) => TPointer(t)),
        inner
      )
    case CArrayDeclarator(_, _, inner) =>
      foldDeclarator(TArray(t), inner)
    case CTypedFunctionDeclarator(params, varargs, inner) => inner match {
      case CName(_) => t
      case _ => ??? // TODO: there is no type (yet) for callables
    }
    case CAnonymousFunctionDeclarator(params, inner) => inner match {
      case CName(_) => t
      case _ => ???
    }
    case CName(_) => t
  }

  /**
   * The return type of a function declaration, or the type of a name
   * @return the (lookalike) type
   */
  def ofDeclarator(specs: Seq[CDeclarationSpecifier], decl: CDeclarator): Type =
    foldDeclarator(CPrimitiveType(specs).lookalike, decl)
}

case class CPrimitiveType(specifiers: Seq[CDeclarationSpecifier])(implicit val o: Origin = DiagnosticOrigin) extends CType {
  /**
   * Lookalike must be distinct from CPrimitiveType. Otherwise, superTypeOf will loop.
   */
  @tailrec
  final def lookalike: Type = specifiers match {
    case Seq(CVoid()) => TVoid()
    case Seq(CChar()) => TChar()
    case t if NUMBER_LIKE_SPECIFIERS.contains(t) => TInt()
    case Seq(CFloat()) | Seq(CDouble()) | Seq(CLong(), CDouble()) => TFloat()
    case Seq(CBool()) => TBool()
    case Seq(defn @ CTypedefName(_)) => defn.ref.get.t match {
      case t @ CPrimitiveType(_) => t.lookalike
      case other => other
    }
  }

  override def superTypeOfImpl(other: Type): Boolean =
    lookalike.superTypeOf(other)

  override def subTypeOfImpl(other: Type): Boolean =
    other.superTypeOf(lookalike)
}