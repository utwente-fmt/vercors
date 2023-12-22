package vct.col.ast.statement.terminal

import vct.col.ast.Unlock
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.UnlockOps

trait UnlockImpl[G] extends UnlockOps[G] { this: Unlock[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("unlock") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}