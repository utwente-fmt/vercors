package vct.col.ast.family.coercion

import vct.col.ast.{CoerceBoundIntFrac, TFraction}
import vct.col.ast.ops.CoerceBoundIntFracOps

trait CoerceBoundIntFracImpl[G] extends CoerceBoundIntFracOps[G] {
  this: CoerceBoundIntFrac[G] =>
  override def target: TFraction[G] = TFraction()
}
