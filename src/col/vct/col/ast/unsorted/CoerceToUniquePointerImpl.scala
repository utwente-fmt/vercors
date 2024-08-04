package vct.col.ast.unsorted

import vct.col.ast.{CoerceToUniquePointer, TUniquePointer, Type}
import vct.col.ast.ops.CoerceToUniquePointerOps
import vct.col.print._

trait CoerceToUniquePointerImpl[G] extends CoerceToUniquePointerOps[G] { this: CoerceToUniquePointer[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  override def target: Type[G] = TUniquePointer(inner, targetId)
}
