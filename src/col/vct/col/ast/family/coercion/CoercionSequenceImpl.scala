package vct.col.ast.family.coercion

import vct.col.ast.{CoercionSequence, Type}
import vct.col.ast.ops.CoercionSequenceOps

trait CoercionSequenceImpl[G] extends CoercionSequenceOps[G] {
  this: CoercionSequence[G] =>
  override def target: Type[G] = coercions.last.target
}
