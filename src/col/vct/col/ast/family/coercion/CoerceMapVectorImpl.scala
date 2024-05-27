package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapVector, TVector}
import vct.col.ast.ops.CoerceMapVectorOps

trait CoerceMapVectorImpl[G] extends CoerceMapVectorOps[G] { this: CoerceMapVector[G] =>
  override def target: TVector[G] = TVector(size, targetVectorElement)()
}
