package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapMap, TMap}
import vct.col.ast.ops.CoerceMapMapOps

trait CoerceMapMapImpl[G] extends CoerceMapMapOps[G] {
  this: CoerceMapMap[G] =>
  def target: TMap[G] = TMap(targetTypes._1, targetTypes._2)
}
