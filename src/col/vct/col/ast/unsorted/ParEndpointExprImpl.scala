package vct.col.ast.unsorted

import vct.col.ast.ParEndpointExpr
import vct.col.ast.ops.ParEndpointExprOps
import vct.col.print._

trait ParEndpointExprImpl[G] extends ParEndpointExprOps[G] {
  this: ParEndpointExpr[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
