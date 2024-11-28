package vct.col.ast.unsorted

import vct.col.ast.NeutralFilterMode
import vct.col.ast.ops.NeutralFilterModeOps
import vct.col.print._

trait NeutralFilterModeImpl[G] extends NeutralFilterModeOps[G] {
  this: NeutralFilterMode[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
