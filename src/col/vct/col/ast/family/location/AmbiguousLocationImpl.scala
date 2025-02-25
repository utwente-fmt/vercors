package vct.col.ast.family.location

import vct.col.ast.AmbiguousLocation
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.AmbiguousLocationOps

trait AmbiguousLocationImpl[G] extends AmbiguousLocationOps[G] {
  this: AmbiguousLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show
}
