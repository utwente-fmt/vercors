package vct.col.ast.unsorted

import vct.col.ast.PVLSeqRun
import vct.col.ast.ops.PVLSeqRunOps
import vct.col.print._

trait PVLSeqRunImpl[G] extends PVLSeqRunOps[G] { this: PVLSeqRun[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
