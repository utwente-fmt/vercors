package vct.col.ast.unsorted

import vct.col.ast.PVLChannelInvariant
import vct.col.ast.ops.PVLChannelInvariantOps
import vct.col.print._

trait PVLChannelInvariantImpl[G] extends PVLChannelInvariantOps[G] {
  this: PVLChannelInvariant[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
