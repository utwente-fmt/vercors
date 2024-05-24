package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullArray, TArray}
import vct.col.ast.ops.CoerceNullArrayOps

trait CoerceNullArrayImpl[G] extends CoerceNullArrayOps[G] { this: CoerceNullArray[G] => 
  override def target: TArray[G] = TArray(arrayElementType)
}
