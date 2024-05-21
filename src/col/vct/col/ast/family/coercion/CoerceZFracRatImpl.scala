package vct.col.ast.family.coercion

import vct.col.ast.{CoerceZFracRat, TRational}
import vct.col.ast.ops.CoerceZFracRatOps

trait CoerceZFracRatImpl[G] extends CoerceZFracRatOps[G] { this: CoerceZFracRat[G] => 
  override def target: TRational[G] = TRational()
}
