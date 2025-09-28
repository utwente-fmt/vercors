package vct.col.ast.family.coercion

import vct.col.ast.ops.CoercePointerNonNullPointerArrayOps
import vct.col.ast.{CoercePointerNonNullPointerArray, TNonNullPointerArray}

trait CoercePointerNonNullPointerArrayImpl[G]
    extends CoercePointerNonNullPointerArrayOps[G] {
  this: CoercePointerNonNullPointerArray[G] =>
  override def target: TNonNullPointerArray[G] =
    TNonNullPointerArray(elementType, dimensions, unique)
}
