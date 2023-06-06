package vct.col.ast.statement.terminal

import vct.col.ast.Unlock
import vct.col.print.{Ctx, Doc, Show, Text}

trait UnlockImpl[G] { this: Unlock[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("unlock") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}