package vct.col.ast.family.coercion

import vct.col.ast.{CoerceZFracRat, TRational}

trait CoerceZFracRatImpl[G] { this: CoerceZFracRat[G] => 
  override def target: TRational[G] = TRational()
}
