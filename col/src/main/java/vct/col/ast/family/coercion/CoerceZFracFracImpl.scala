package vct.col.ast.family.coercion

import vct.col.ast.{CoerceZFracFrac, TFraction}

trait CoerceZFracFracImpl[G] { this: CoerceZFracFrac[G] =>
  override def target: TFraction[G] = TFraction()
}
