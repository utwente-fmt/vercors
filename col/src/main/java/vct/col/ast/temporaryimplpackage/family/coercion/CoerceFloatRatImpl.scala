package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceFloatRat, TRational}

trait CoerceFloatRatImpl[G] { this: CoerceFloatRat[G] => 
  override def target: TRational[G] = TRational()
}
