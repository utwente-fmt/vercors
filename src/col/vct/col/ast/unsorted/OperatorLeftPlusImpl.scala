package vct.col.ast.unsorted

import vct.col.ast.OperatorLeftPlus
import vct.col.ast.ops.OperatorLeftPlusOps
import vct.col.print._

trait OperatorLeftPlusImpl[G] extends OperatorLeftPlusOps[G] {
  this: OperatorLeftPlus[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
