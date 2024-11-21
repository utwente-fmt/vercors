package vct.col.ast.unsorted

import vct.col.ast.{PVLEndpointSetExpr, Type}
import vct.col.ast.ops.PVLEndpointSetExprOps
import vct.col.print._

trait PVLEndpointSetExprImpl[G] extends PVLEndpointSetExprOps[G] {
  this: PVLEndpointSetExpr[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  override def t: Type[G] = ???
}
