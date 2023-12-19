package vct.col.ast.statement.terminal

import vct.col.ast.Notify
import vct.col.print.{Ctx, Doc, Show, Text}

trait NotifyImpl[G] { this: Notify[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("notify") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}