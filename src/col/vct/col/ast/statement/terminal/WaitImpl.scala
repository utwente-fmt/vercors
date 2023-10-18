package vct.col.ast.statement.terminal

import vct.col.ast.Wait
import vct.col.print.{Ctx, Doc, Show, Text}

trait WaitImpl[G] {
  this: Wait[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("wait") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}
