package vct.col.ast.statement.terminal

import vct.col.ast.DefaultCase
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.DefaultCaseOps

trait DefaultCaseImpl[G] extends DefaultCaseOps[G] { this: DefaultCase[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("default:")
}