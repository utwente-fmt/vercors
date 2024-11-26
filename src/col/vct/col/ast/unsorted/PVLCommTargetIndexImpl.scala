package vct.col.ast.unsorted

import vct.col.ast.{PVLCommTargetIndex, PVLCommTargetEndpoint}
import vct.col.ast.ops.PVLCommTargetIndexOps
import vct.col.print._

trait PVLCommTargetIndexImpl[G] extends PVLCommTargetIndexOps[G] {
  this: PVLCommTargetIndex[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  require(this.name match {
    case _: PVLCommTargetEndpoint[G] => true
    case _ => false
  })
}
