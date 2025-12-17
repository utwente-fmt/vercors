package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceConstPointerArrayPointerOps
import vct.col.ast.{CoerceConstPointerArrayPointer, TConstPointer}

trait CoerceConstPointerArrayPointerImpl[G]
    extends CoerceConstPointerArrayPointerOps[G] {
  this: CoerceConstPointerArrayPointer[G] =>
  override def target: TConstPointer[G] = TConstPointer(elementType)
}
