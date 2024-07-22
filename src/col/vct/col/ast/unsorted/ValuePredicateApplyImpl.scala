package vct.col.ast.unsorted

import vct.col.ast.ValuePredicateApply
import vct.col.ast.ops.ValuePredicateApplyOps
import vct.col.print._

trait ValuePredicateApplyImpl[G] extends ValuePredicateApplyOps[G] {
  this: ValuePredicateApply[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
