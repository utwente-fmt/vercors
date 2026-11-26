package vct.col.ast.unsorted

import vct.col.ast.GhostFunction
import vct.col.ast.ops.GhostFunctionOps
import vct.col.print._

trait GhostFunctionImpl[G] extends GhostFunctionOps[G] {
  this: GhostFunction[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
