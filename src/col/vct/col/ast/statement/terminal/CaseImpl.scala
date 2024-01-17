package vct.col.ast.statement.terminal

import vct.col.ast.Case
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.CaseOps

trait CaseImpl[G] extends CaseOps[G] { this: Case[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("case") <+> pattern <> ":"
}