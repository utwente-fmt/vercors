package vct.col.ast.family.location

import vct.col.ast.AmbiguousLocation
import vct.col.print.{Ctx, Doc, Text}

trait AmbiguousLocationImpl[G] { this: AmbiguousLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show
}
