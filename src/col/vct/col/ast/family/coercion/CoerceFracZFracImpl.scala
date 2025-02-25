package vct.col.ast.family.coercion

import vct.col.ast.{CoerceFracZFrac, TZFraction}
import vct.col.ast.ops.CoerceFracZFracOps

trait CoerceFracZFracImpl[G] extends CoerceFracZFracOps[G] {
  this: CoerceFracZFrac[G] =>
  override def target: TZFraction[G] = TZFraction()
}
