package vct.col.ast.statement.terminal

import vct.col.ast.Send
import vct.col.print.{Ctx, Doc, Show, Text, Group}

trait SendImpl[G] { this: Send[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("send") <+> ctx.name(decl) <> "," <+> delta.toString <> ":" <>> res <> ";")

  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}