package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceNonNullPointerOps
import vct.col.ast.{CoerceNonNullPointer, TPointer}

trait CoerceNonNullPointerImpl[G] extends CoerceNonNullPointerOps[G] {
  this: CoerceNonNullPointer[G] =>
  override def target: TPointer[G] = TPointer(elementType)
}
