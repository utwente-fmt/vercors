package vct.col.ast.family.coercion

import vct.col.ast.{CoerceZFracFrac, TFraction}
import vct.col.ast.ops.CoerceZFracFracOps

trait CoerceZFracFracImpl[G] extends CoerceZFracFracOps[G] { this: CoerceZFracFrac[G] =>
  override def target: TFraction[G] = TFraction()
}
