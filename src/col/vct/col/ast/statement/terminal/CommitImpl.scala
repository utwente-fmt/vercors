package vct.col.ast.statement.terminal

import vct.col.ast.Commit
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.CommitOps

trait CommitImpl[G] extends CommitOps[G] { this: Commit[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("commit") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}