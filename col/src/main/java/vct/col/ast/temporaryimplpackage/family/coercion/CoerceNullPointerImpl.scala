package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceNullPointer, TPointer}

trait CoerceNullPointerImpl[G] { this: CoerceNullPointer[G] => 
  override def target: TPointer[G] = TPointer(pointerElementType)
}
