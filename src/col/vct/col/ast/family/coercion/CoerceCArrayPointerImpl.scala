package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCArrayPointer, TPointer}

trait CoerceCArrayPointerImpl[G] { this: CoerceCArrayPointer[G] =>
  override def target: TPointer[G] = TPointer(elementType)
}
