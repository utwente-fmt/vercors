package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceMapTuple, TTuple}

trait CoerceMapTupleImpl[G] { this: CoerceMapTuple[G] => 
  override def target: TTuple[G] = TTuple(targetTypes)
}
