package vct.col.ast.lang.pvl

import vct.col.ast.declaration.cls.ClassDeclarationImpl
import vct.col.ast.node.NodeImpl
import vct.col.ast.{Declaration, PVLEndpoint, PVLLocal, RangeBinder, TClass}
import vct.col.ast.ops.PVLEndpointOps
import vct.col.check.CheckContext

trait PVLEndpointImpl[G]
    extends PVLEndpointOps[G] with ClassDeclarationImpl[G] {
  this: PVLEndpoint[G] =>

  // override def layout(implicit ctx: Ctx): Doc = ???

  def t: TClass[G] = cls.decl.classType(typeArgs)

  override def declarations: Seq[Declaration[G]] =
    range.map(r => Seq(r.binder)).getOrElse(Seq())

  def isFamily: Boolean = range.nonEmpty
  def isSingle: Boolean = !isFamily

}
