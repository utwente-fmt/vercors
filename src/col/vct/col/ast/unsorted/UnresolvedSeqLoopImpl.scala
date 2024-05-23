package vct.col.ast.unsorted

import vct.col.ast.UnresolvedSeqLoop
import vct.col.ast.ops.UnresolvedSeqLoopOps
import vct.col.print._

trait UnresolvedSeqLoopImpl[G] extends UnresolvedSeqLoopOps[G] { this: UnresolvedSeqLoop[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
