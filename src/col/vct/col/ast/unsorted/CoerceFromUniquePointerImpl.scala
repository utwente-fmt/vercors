package vct.col.ast.unsorted

import vct.col.ast.CoerceFromUniquePointer
import vct.col.ast.ops.CoerceFromUniquePointerOps
import vct.col.print._

trait CoerceFromUniquePointerImpl[G] extends CoerceFromUniquePointerOps[G] { this: CoerceFromUniquePointer[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
