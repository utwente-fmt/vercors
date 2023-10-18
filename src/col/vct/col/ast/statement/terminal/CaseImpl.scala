package vct.col.ast.statement.terminal

import vct.col.ast.Case
import vct.col.print.{Ctx, Doc, Show, Text}

trait CaseImpl[G] {
  this: Case[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("case") <+> pattern <> ":"
}
