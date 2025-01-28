package vct.col.ast.lang.pvl

import vct.col.ast.PVLLoop
import vct.col.ast.ops.PVLLoopOps

trait PVLLoopImpl[G] extends PVLLoopOps[G] {
  this: PVLLoop[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
