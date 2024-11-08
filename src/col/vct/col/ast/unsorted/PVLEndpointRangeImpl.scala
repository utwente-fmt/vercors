package vct.col.ast.unsorted

import vct.col.ast.PVLEndpointRange
import vct.col.ast.ops.PVLEndpointRangeOps
import vct.col.print._

trait PVLEndpointRangeImpl[G] extends PVLEndpointRangeOps[G] { this: PVLEndpointRange[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
