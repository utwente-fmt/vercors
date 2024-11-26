package vct.col.ast.unsorted

import vct.col.ast.{PVLCommTargetEndpoint, PVLCommTargetRange, RangeBinder}
import vct.col.ast.ops.PVLCommTargetRangeOps
import vct.col.print._

trait PVLCommTargetRangeImpl[G] extends PVLCommTargetRangeOps[G] {
  this: PVLCommTargetRange[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  require(this.name match {
    case _: PVLCommTargetEndpoint[G] => true
    case _ => false
  })
}
