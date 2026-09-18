package vct.col.ast.family.coercion

import vct.col.ast.CoerceIncreasePrecision
import vct.col.ast.ops.CoerceIncreasePrecisionOps
import vct.col.print._

trait CoerceIncreasePrecisionImpl[G] extends CoerceIncreasePrecisionOps[G] {
  this: CoerceIncreasePrecision[G] =>
}
