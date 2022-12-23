package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullArray, TArray}

trait CoerceNullArrayImpl[G] { this: CoerceNullArray[G] => 
  override def target: TArray[G] = TArray(arrayElementType)
}
