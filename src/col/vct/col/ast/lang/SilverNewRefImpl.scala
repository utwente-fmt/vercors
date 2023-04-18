package vct.col.ast.lang

import vct.col.ast.SilverNewRef
import vct.col.print.{Ctx, Doc, Group, Text}

trait SilverNewRefImpl[G] { this: SilverNewRef[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(ctx.name(v)) <+> ":=" <+> "new(" <> Doc.args(fields.map(ctx.name).map(Text)) <> ")")
}