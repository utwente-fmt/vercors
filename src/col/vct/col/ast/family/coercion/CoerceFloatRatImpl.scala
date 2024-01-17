package vct.col.ast.family.coercion

import vct.col.ast.{CoerceFloatRat, TRational}
import vct.col.ast.ops.CoerceFloatRatOps

trait CoerceFloatRatImpl[G] extends CoerceFloatRatOps[G] { this: CoerceFloatRat[G] => 
  override def target: TRational[G] = TRational()
}
