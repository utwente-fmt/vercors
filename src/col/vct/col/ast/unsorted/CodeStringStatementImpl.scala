package vct.col.ast.unsorted

import vct.col.ast.CodeStringStatement
import vct.col.ast.ops.CodeStringStatementOps
import vct.col.print._

trait CodeStringStatementImpl[G] extends CodeStringStatementOps[G] { this: CodeStringStatement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
