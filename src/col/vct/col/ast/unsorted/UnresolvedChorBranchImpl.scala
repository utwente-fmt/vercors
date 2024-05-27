package vct.col.ast.unsorted

import vct.col.ast.UnresolvedChorBranch
import vct.col.print._
import vct.col.ast.ops.UnresolvedChorBranchOps

trait UnresolvedChorBranchImpl[G] extends UnresolvedChorBranchOps[G] {
  this: UnresolvedChorBranch[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
