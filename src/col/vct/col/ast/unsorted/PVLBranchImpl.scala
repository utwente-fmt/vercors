package vct.col.ast.unsorted

import vct.col.ast.PVLBranch
import vct.col.ast.ops.PVLBranchOps
import vct.col.print._

trait PVLBranchImpl[G] extends PVLBranchOps[G] {
  this: PVLBranch[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
