package vct.col.ast.statement.composite

import vct.col.ast.ParStatement
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.ParStatementOps

trait ParStatementImpl[G] extends ParStatementOps[G] {
  this: ParStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = impl.show
}
