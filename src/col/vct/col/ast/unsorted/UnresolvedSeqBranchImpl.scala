package vct.col.ast.unsorted

import vct.col.ast.UnresolvedSeqBranch
import vct.col.ast.ops.UnresolvedSeqBranchOps
import vct.col.print._

trait UnresolvedSeqBranchImpl[G] extends UnresolvedSeqBranchOps[G] { this: UnresolvedSeqBranch[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
