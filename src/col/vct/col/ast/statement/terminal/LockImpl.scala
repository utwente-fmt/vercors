package vct.col.ast.statement.terminal

import vct.col.ast.Lock
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.LockOps

trait LockImpl[G] extends LockOps[G] { this: Lock[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("lock") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}