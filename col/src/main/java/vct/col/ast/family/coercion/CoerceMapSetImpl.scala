package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapSet, TSet}

trait CoerceMapSetImpl[G] { this: CoerceMapSet[G] => 
  override def target: TSet[G] = TSet(targetSetElement)
}
