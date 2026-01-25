package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceImmutablePointerArrayPointerOps
import vct.col.ast.{CoerceImmutablePointerArrayPointer, TImmutablePointer}

trait CoerceImmutablePointerArrayPointerImpl[G]
    extends CoerceImmutablePointerArrayPointerOps[G] {
  this: CoerceImmutablePointerArrayPointer[G] =>
  override def target: TImmutablePointer[G] = TImmutablePointer(elementType)
}
