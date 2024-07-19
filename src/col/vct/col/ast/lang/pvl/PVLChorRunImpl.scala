package vct.col.ast.lang.pvl

import vct.col.ast.PVLChorRun
import vct.col.ast.ops.PVLChorRunOps

trait PVLChorRunImpl[G] extends PVLChorRunOps[G] {
  this: PVLChorRun[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
