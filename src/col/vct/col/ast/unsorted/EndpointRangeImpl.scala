package vct.col.ast.unsorted

import vct.col.ast.EndpointRange
import vct.col.ast.ops.EndpointRangeOps
import vct.col.print._

trait EndpointRangeImpl[G] extends EndpointRangeOps[G] {
  this: EndpointRange[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
