package vct.col.ast.declaration.singular

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{
  Declaration,
  Endpoint,
  RangeBinder,
  TByReferenceClass,
  TClass,
  Type,
}
import vct.col.print._
import vct.col.ast.ops.{EndpointFamilyOps, EndpointOps}
import vct.col.check.{CheckContext, CheckError}

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
}
