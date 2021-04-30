package vct.col.ast

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
case class CTypedefName(name: String)(implicit val o: Origin) extends CTypeSpecifier
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
case class CDeclarationStatement(decl: CDeclaration)(implicit val o: Origin) extends CStatement with NoCheck
case class CLabeledStatement(label: LabelDecl, statement: Statement)(implicit val o: Origin) extends CStatement with NoCheck
case class CGoto(label: Ref[LabelDecl])(implicit val o: Origin) extends CStatement with NoCheck

case class GpgpuLocalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin)
  extends Check(requires.checkSubType(TResource()), ensures.checkSubType(TResource())) with CStatement
case class GpgpuGlobalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin)
  extends Check(requires.checkSubType(TResource()), ensures.checkSubType(TResource())) with CStatement
case class GpgpuAtomic(impl: Statement, before: Statement, after: Statement)(implicit val o: Origin) extends CStatement with NoCheck

sealed trait CExpr extends ExtraExpr
case class CInvocation(applicable: Expr, args: Seq[Expr])(implicit val o: Origin) extends CExpr with NoCheck {
  override def t: Type = TSkip()
}
case class CStructAccess(struct: Expr, field: Ref[Declaration])(implicit val o: Origin) extends CExpr with NoCheck {
  override def t: Type = TSkip()
}
case class CStructDeref(struct: Expr, field: Ref[Declaration])(implicit val o: Origin) extends CExpr with NoCheck {
  override def t: Type = TSkip()
}
case class GpgpuCudaKernelInvocation(kernel: Ref[CAbstractGlobalDeclaration], blocks: Expr, threads: Expr, args: Seq[Expr])(implicit val o: Origin) extends CExpr with NoCheck {
  override def t: Type = TSkip()
}

sealed trait CType extends ExtraType
case class CPrimitiveType(specifiers: Seq[CDeclarationSpecifier])(implicit val o: Origin = DiagnosticOrigin) extends CType {
  override def superTypeOf(other: Type): Boolean = ???
}