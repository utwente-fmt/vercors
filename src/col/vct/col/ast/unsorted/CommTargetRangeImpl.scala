package vct.col.ast.unsorted

import vct.col.ast.CommTargetRange
import vct.col.print._
import vct.col.ast.ops.CommTargetRangeOps

trait CommTargetRangeImpl[G] extends CommTargetRangeOps[G] {
  this: CommTargetRange[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
