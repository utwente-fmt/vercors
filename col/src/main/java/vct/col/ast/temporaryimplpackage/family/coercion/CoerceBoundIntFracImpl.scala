package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceBoundIntFrac, TFraction}

trait CoerceBoundIntFracImpl[G] { this: CoerceBoundIntFrac[G] =>
  override def target: TFraction[G] = TFraction()
}
