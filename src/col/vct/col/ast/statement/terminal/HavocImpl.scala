package vct.col.ast.statement.terminal

import vct.col.ast.Havoc
import vct.col.print.{Ctx, Doc, Show, Text}

trait HavocImpl[G] { this: Havoc[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("havoc") <+> loc <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}