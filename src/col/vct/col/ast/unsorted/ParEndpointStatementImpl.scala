package vct.col.ast.unsorted

import vct.col.ast.ParEndpointStatement
import vct.col.ast.ops.ParEndpointStatementOps
import vct.col.print._

trait ParEndpointStatementImpl[G] extends ParEndpointStatementOps[G] {
  this: ParEndpointStatement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
