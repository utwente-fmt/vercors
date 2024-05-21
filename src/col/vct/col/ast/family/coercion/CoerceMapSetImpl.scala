package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapSet, TSet}
import vct.col.ast.ops.CoerceMapSetOps

trait CoerceMapSetImpl[G] extends CoerceMapSetOps[G] { this: CoerceMapSet[G] => 
  override def target: TSet[G] = TSet(targetSetElement)
}
