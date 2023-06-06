package vct.col.ast.lang

import vct.col.ast.SilverPartialTAxiomatic
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverPartialTAxiomaticImpl[G] { this: SilverPartialTAxiomatic[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref)) <> open <> "..." <> close
}
