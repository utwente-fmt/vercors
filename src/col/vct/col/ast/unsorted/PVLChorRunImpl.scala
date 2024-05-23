package vct.col.ast.unsorted

import vct.col.ast.PVLChorRun
import vct.col.ast.ops.PVLChorRunOps
import vct.col.print._

trait PVLChorRunImpl[G] extends PVLChorRunOps[G] {
  this: PVLChorRun[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
