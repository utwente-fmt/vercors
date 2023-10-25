package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCPPArrayPointer, TPointer}

trait CoerceCPPArrayPointerImpl[G] { this: CoerceCPPArrayPointer[G] =>
  override def target: TPointer[G] = TPointer(elementType)
}