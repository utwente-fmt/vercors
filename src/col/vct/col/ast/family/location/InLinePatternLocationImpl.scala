package vct.col.ast.family.location

import vct.col.ast.InLinePatternLocation
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.InLinePatternLocationOps

trait InLinePatternLocationImpl[G] extends InLinePatternLocationOps[G] { this: InLinePatternLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc = loc.show <+> "{" <> pattern <> "}"
}
