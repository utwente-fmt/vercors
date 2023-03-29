package vct.col.ast.family.coercion

import vct.col.ast.{CoerceRatZFrac, TZFraction}

trait CoerceRatZFracImpl[G] { this: CoerceRatZFrac[G] =>
  override def target: TZFraction[G] = TZFraction()
}
