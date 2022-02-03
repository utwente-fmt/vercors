package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceBoundIntZFrac, TZFraction}

trait CoerceBoundIntZFracImpl[G] { this: CoerceBoundIntZFrac[G] =>
  override def target: TZFraction[G] = TZFraction()
}
