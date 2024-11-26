package vct.col.ast.unsorted

import vct.col.ast.{PVLCommTargetExpr, Type}
import vct.col.print._
import vct.col.ast.ops.PVLCommTargetExprOps

trait PVLCommTargetExprImpl[G] extends PVLCommTargetExprOps[G] {
  this: PVLCommTargetExpr[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  override def t: Type[G] = ???
}
