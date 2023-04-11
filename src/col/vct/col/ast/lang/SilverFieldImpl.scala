package vct.col.ast.lang

import vct.col.ast.SilverField
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverFieldImpl[G] { this: SilverField[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("field") <+> ctx.name(this) <> ":" <+> t
}