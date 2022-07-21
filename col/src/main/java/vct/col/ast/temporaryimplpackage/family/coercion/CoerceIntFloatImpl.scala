package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceIntFloat, TFloat}

trait CoerceIntFloatImpl[G] { this: CoerceIntFloat[G] =>
  override def target: TFloat[G] = TFloat()
}
