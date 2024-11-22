package vct.col.ast.unsorted

import vct.col.ast.{PVLEndpointName, PVLEndpointRange, RangeBinder}
import vct.col.ast.ops.PVLEndpointRangeOps
import vct.col.print._

trait PVLEndpointRangeImpl[G] extends PVLEndpointRangeOps[G] {
  this: PVLEndpointRange[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  require(this.name match {
    case _: PVLEndpointName[G] => true
    case _ => false
  })
  require(this.range match {
    case _: RangeBinder[G] => true
    case _ => false
  })
}
