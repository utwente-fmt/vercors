package vct.col.ast.unsorted

import vct.col.ast.WrapperFunction
import vct.col.ast.ops.WrapperFunctionOps
import vct.col.print._

trait WrapperFunctionImpl[G] extends WrapperFunctionOps[G] {
  this: WrapperFunction[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
