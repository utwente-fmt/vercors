package vct.col.ast.statement.terminal

import vct.col.ast.Fork
import vct.col.print.{Ctx, Doc, Show, Text}

trait ForkImpl[G] {
  this: Fork[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("fork") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}
