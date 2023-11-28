package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapTuple, TTuple}
import vct.col.ast.ops.CoerceMapTupleOps

trait CoerceMapTupleImpl[G] extends CoerceMapTupleOps[G] { this: CoerceMapTuple[G] => 
  override def target: TTuple[G] = TTuple(targetTypes)
}
