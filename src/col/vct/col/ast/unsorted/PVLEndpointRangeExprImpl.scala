package vct.col.ast.unsorted

import vct.col.ast.{PVLEndpointRangeExpr, Type}
import vct.col.ast.ops.PVLEndpointRangeExprOps
import vct.col.print._

trait PVLEndpointRangeExprImpl[G] extends PVLEndpointRangeExprOps[G] {
  this: PVLEndpointRangeExpr[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  def t: Type[G] = ???
}
