package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceNullArray, TArray}

trait CoerceNullArrayImpl[G] { this: CoerceNullArray[G] => 
  override def target: TArray[G] = TArray(arrayElementType)
}
