package vct.col.ast.lang.pvl

import vct.col.ast.PVLBranch
import vct.col.ast.ops.PVLBranchOps

trait PVLBranchImpl[G] extends PVLBranchOps[G] {
  this: PVLBranch[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
