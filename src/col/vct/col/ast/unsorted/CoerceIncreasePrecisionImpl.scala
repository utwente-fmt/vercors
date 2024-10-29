package vct.col.ast.unsorted

import vct.col.ast.CoerceIncreasePrecision
import vct.col.ast.ops.CoerceIncreasePrecisionOps
import vct.col.print._

trait CoerceIncreasePrecisionImpl[G] extends CoerceIncreasePrecisionOps[G] {
  this: CoerceIncreasePrecision[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
