package vct.col.ast.lang

import vct.col.ast.SilverLocalAssign
import vct.col.print.{Ctx, Doc, Text}

trait SilverLocalAssignImpl[G] { this: SilverLocalAssign[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(v)) <+> ":=" <+> value
}