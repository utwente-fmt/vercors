package vct.col.ast.family.coercion

import vct.col.ast.{CoerceRatZFrac, TZFraction}
import vct.col.ast.ops.CoerceRatZFracOps

trait CoerceRatZFracImpl[G] extends CoerceRatZFracOps[G] { this: CoerceRatZFrac[G] =>
  override def target: TZFraction[G] = TZFraction()
}
