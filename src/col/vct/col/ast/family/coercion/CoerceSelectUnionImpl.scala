package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSelectUnion, TUnion}
import vct.col.ast.ops.CoerceSelectUnionOps

trait CoerceSelectUnionImpl[G] extends CoerceSelectUnionOps[G] {
  this: CoerceSelectUnion[G] =>
  override def target: TUnion[G] = TUnion(targetAlts)
}
