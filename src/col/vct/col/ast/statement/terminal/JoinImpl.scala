package vct.col.ast.statement.terminal

import vct.col.ast.Join
import vct.col.print.{Ctx, Doc, Show, Text}

trait JoinImpl[G] { this: Join[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("join") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}