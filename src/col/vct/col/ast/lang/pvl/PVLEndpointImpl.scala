package vct.col.ast.lang.pvl

import vct.col.ast.declaration.cls.ClassDeclarationImpl
import vct.col.ast.node.NodeImpl
import vct.col.ast.{Declaration, Node, PVLEndpoint, PVLFamily, TClass}
import vct.col.ast.ops.PVLEndpointOps
import vct.col.check.CheckContext

trait PVLEndpointImpl[G]
    extends PVLEndpointOps[G] with ClassDeclarationImpl[G] {
  this: PVLEndpoint[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def t: TClass[G] = cls.decl.classType(typeArgs)

  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] = context.withScope(declarations)

  override def declarations: Seq[Declaration[G]] =
    range.map(f => Seq(f.binder)).getOrElse(Seq())

  def isFamily: Boolean = range.nonEmpty
  def isEndpoint: Boolean = !isFamily
}
