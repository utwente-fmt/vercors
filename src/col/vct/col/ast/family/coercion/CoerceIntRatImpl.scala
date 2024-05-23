package vct.col.ast.family.coercion

import vct.col.ast.{CoerceIntRat, TRational}
import vct.col.ast.ops.CoerceIntRatOps

trait CoerceIntRatImpl[G] extends CoerceIntRatOps[G] { this: CoerceIntRat[G] =>
  override def target: TRational[G] = TRational()
}
