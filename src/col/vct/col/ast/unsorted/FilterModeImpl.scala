package vct.col.ast.unsorted

import vct.col.ast.FilterMode
import vct.col.ast.ops.FilterModeFamilyOps
import vct.col.print._

trait FilterModeImpl[G] extends FilterModeFamilyOps[G] {
  this: FilterMode[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
