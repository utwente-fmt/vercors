package vct.col.ast.unsorted

import vct.col.ast.GhostWrapperFunction
import vct.col.ast.ops.GhostWrapperFunctionOps

trait GhostWrapperFunctionImpl[G] extends GhostWrapperFunctionOps[G] {
  this: GhostWrapperFunction[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
