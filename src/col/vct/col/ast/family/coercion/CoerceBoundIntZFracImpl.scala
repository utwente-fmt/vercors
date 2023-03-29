package vct.col.ast.family.coercion

import vct.col.ast.{CoerceBoundIntZFrac, TZFraction}

trait CoerceBoundIntZFracImpl[G] { this: CoerceBoundIntZFrac[G] =>
  override def target: TZFraction[G] = TZFraction()
}
