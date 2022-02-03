package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceZFracRat, TRational}

trait CoerceZFracRatImpl[G] { this: CoerceZFracRat[G] => 
  override def target: TRational[G] = TRational()
}
