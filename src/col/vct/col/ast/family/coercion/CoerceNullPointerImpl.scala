package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullPointer, TPointer}
import vct.col.ast.ops.CoerceNullPointerOps

trait CoerceNullPointerImpl[G] extends CoerceNullPointerOps[G] {
  this: CoerceNullPointer[G] =>
  override def target: TPointer[G] = TPointer(pointerElementType)
}
