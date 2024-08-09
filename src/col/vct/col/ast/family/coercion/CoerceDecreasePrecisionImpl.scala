package vct.col.ast.family.coercion

import vct.col.ast.CoerceDecreasePrecision
import vct.col.ast.ops.CoerceDecreasePrecisionOps
import vct.col.print._

trait CoerceDecreasePrecisionImpl[G] extends CoerceDecreasePrecisionOps[G] {
  this: CoerceDecreasePrecision[G] =>
}
