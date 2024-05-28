package vct.col.ast.family.coercion

import vct.col.ast.{CoerceBoundIntZFrac, TZFraction}
import vct.col.ast.ops.CoerceBoundIntZFracOps

trait CoerceBoundIntZFracImpl[G] extends CoerceBoundIntZFracOps[G] {
  this: CoerceBoundIntZFrac[G] =>
  override def target: TZFraction[G] = TZFraction()
}
