package vct.col.ast.unsorted

import vct.col.ast.{PVLEndpoint, TClass, Type}
import vct.col.ast.ops.PVLEndpointOps
import vct.col.print._

trait PVLEndpointImpl[G] extends PVLEndpointOps[G] {
  this: PVLEndpoint[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def t: TClass[G] = TClass(cls, typeArgs)
}
