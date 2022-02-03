package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceIntRat, TRational}

trait CoerceIntRatImpl[G] { this: CoerceIntRat[G] =>
  override def target: TRational[G] = TRational()
}
