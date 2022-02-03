package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoercionSequence, Type}

trait CoercionSequenceImpl[G] { this: CoercionSequence[G] =>
  override def target: Type[G] = coercions.last.target
}