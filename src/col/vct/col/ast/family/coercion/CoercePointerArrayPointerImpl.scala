package vct.col.ast.family.coercion

import vct.col.ast.ops.CoercePointerArrayPointerOps
import vct.col.ast.{CoercePointerArrayPointer, TPointer}

trait CoercePointerArrayPointerImpl[G] extends CoercePointerArrayPointerOps[G] {
  this: CoercePointerArrayPointer[G] =>
  override def target: TPointer[G] = TPointer(elementType, unique)
}
