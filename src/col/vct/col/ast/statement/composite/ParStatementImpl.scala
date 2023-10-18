package vct.col.ast.statement.composite

import vct.col.ast.ParStatement
import vct.col.print.{Ctx, Doc, Group, Text}

trait ParStatementImpl[G] {
  this: ParStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = impl.show
}
