package vct.col.ast.family.location

import vct.col.ast.InLinePatternLocation
import vct.col.print.{Ctx, Doc, Text}

trait InLinePatternLocationImpl[G] { this: InLinePatternLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc = loc.show <+> "{" <> pattern <> "}"
}
