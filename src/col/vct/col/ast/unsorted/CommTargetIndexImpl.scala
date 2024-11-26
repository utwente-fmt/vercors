package vct.col.ast.unsorted

import vct.col.ast.CommTargetIndex
import vct.col.print._
import vct.col.ast.ops.CommTargetIndexOps

trait CommTargetIndexImpl[G] extends CommTargetIndexOps[G] {
  this: CommTargetIndex[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
