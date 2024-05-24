package vct.col.ast.unsorted

import vct.col.ast.PVLLoop
import vct.col.ast.ops.PVLLoopOps
import vct.col.print._

trait PVLLoopImpl[G] extends PVLLoopOps[G] { this: PVLLoop[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
