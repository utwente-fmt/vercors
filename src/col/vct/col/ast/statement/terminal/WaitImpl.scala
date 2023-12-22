package vct.col.ast.statement.terminal

import vct.col.ast.Wait
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.WaitOps

trait WaitImpl[G] extends WaitOps[G] { this: Wait[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("wait") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}