package vct.col.ast.unsorted

import vct.col.ast.CoerceDecreasePrecision
import vct.col.ast.ops.CoerceDecreasePrecisionOps
import vct.col.print._

trait CoerceDecreasePrecisionImpl[G] extends CoerceDecreasePrecisionOps[G] { this: CoerceDecreasePrecision[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
