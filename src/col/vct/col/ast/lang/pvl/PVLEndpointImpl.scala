package vct.col.ast.lang.pvl

import vct.col.ast.ops.PVLEndpointOps
import vct.col.ast.{PVLEndpoint, TClass}

trait PVLEndpointImpl[G] extends PVLEndpointOps[G] {
  this: PVLEndpoint[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def t: TClass[G] = TClass(cls, typeArgs)
}
