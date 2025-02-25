package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapType, TType}
import vct.col.ast.ops.CoerceMapTypeOps

trait CoerceMapTypeImpl[G] extends CoerceMapTypeOps[G] {
  this: CoerceMapType[G] =>
  def target: TType[G] = TType(targetBound)
}
