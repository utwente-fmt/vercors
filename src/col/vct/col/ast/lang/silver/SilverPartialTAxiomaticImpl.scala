package vct.col.ast.lang.silver

import vct.col.ast.SilverPartialTAxiomatic
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverPartialTAxiomaticOps

trait SilverPartialTAxiomaticImpl[G] extends SilverPartialTAxiomaticOps[G] {
  this: SilverPartialTAxiomatic[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref)) <> open <> "..." <> close
}
