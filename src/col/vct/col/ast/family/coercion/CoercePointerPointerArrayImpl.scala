package vct.col.ast.family.coercion

import vct.col.ast.ops.CoercePointerPointerArrayOps
import vct.col.ast.{CoercePointerPointerArray, TPointerArray}

trait CoercePointerPointerArrayImpl[G] extends CoercePointerPointerArrayOps[G] {
  this: CoercePointerPointerArray[G] =>
  override def target: TPointerArray[G] =
    TPointerArray(elementType, dimensions, unique)
}
