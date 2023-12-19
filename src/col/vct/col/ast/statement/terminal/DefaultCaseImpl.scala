package vct.col.ast.statement.terminal

import vct.col.ast.DefaultCase
import vct.col.print.{Ctx, Doc, Show, Text}

trait DefaultCaseImpl[G] { this: DefaultCase[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("default:")
}