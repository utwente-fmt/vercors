package vct.col.ast.unsorted

import vct.col.ast.CUniquePointerField
import vct.col.ast.ops.CUniquePointerFieldOps
import vct.col.print._

trait CUniquePointerFieldImpl[G] extends CUniquePointerFieldOps[G] { this: CUniquePointerField[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
