package vct.col.ast.declaration.singular

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetRange,
  CommunicateTarget,
  Declaration,
  Endpoint,
  EndpointFamilyExpr,
  RangeBinder,
  Size,
  TByReferenceClass,
  TClass,
  TInt,
  TSeq,
  Type,
  Variable,
}
import vct.col.print._
import vct.col.ast.ops.{EndpointFamilyExprOps, EndpointOps}
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstMatchHelpers.{EndpointName, EndpointRange}
import vct.col.ast.ops.{EndpointFamilyOps, EndpointOps}

trait EndpointImpl[G]
    extends EndpointOps[G] with DeclarationImpl[G] with EndpointFamilyOps[G] {
  this: Endpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("endpoint") <+> ctx.name(this) <+> "=" <+> init)

  def singleType: TClass[G] = cls.decl.classType(typeArgs)
  def rangeType: TSeq[G] = TSeq(singleType)

  override def declarations: Seq[Declaration[G]] =
    range.map(r => Seq(r.binder)).getOrElse(Seq())

  def isFamily: Boolean = range.nonEmpty
  def isSingle: Boolean = !isFamily

  def commTarget: CommunicateTarget[G] = {
    implicit val o = DiagnosticOrigin
    range match {
      case None => CommTargetEndpoint(this.ref)
      case Some(_) =>
        CommTargetRange(
          this.ref,
          RangeBinder(
            new Variable(TInt()),
            AstBuildHelpers.const(0),
            Size(EndpointFamilyExpr(this.ref)),
          ),
        )
    }
  }
}
