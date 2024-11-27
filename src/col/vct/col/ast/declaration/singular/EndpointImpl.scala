package vct.col.ast.declaration.singular

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetRange,
  CommunicateTarget,
  Declaration,
  Endpoint,
  EndpointFamilyLength,
  RangeBinder,
  TByReferenceClass,
  TClass,
  Type,
  Variable,
}
import vct.col.print._
import vct.col.ast.ops.{EndpointFamilyOps, EndpointOps}
import vct.col.check.{CheckContext, CheckError}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstMatchHelpers.{EndpointName, EndpointRange}

trait EndpointImpl[G]
    extends EndpointOps[G] with EndpointFamilyOps[G] with DeclarationImpl[G] {
  this: Endpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("endpoint") <+> ctx.name(this) <+> "=" <+> init)

  def t: TClass[G] = cls.decl.classType(typeArgs)

  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] = context.withScope(declarations)

  override def declarations: Seq[Declaration[G]] =
    getRange.map(r => Seq(r.binder)).getOrElse(Seq())

  def isFamily: Boolean = range.nonEmpty
  def isEndpoint: Boolean = !isFamily

  def getRange: Option[RangeBinder[G]] =
    this.range.map(_.asInstanceOf[RangeBinder[G]])

  def commTarget: CommunicateTarget[G] =
    range match {
      case None => CommTargetEndpoint(this.ref)
      case Some(_) =>
        CommTargetRange(
          this.ref,
          RangeBinder(
            new Variable(),
            AstBuildHelpers.ZERO,
            EndpointFamilyLength(this.ref),
          ),
        )
    }
}
