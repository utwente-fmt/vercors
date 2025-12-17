package vct.col.ast.unsorted

import vct.col.ast.NormalFunction
import vct.col.ast.ops.NormalFunctionOps
import vct.col.print._

trait NormalFunctionImpl[G] extends NormalFunctionOps[G] {
  this: NormalFunction[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
