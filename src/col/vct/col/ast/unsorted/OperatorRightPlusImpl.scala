package vct.col.ast.unsorted

import vct.col.ast.OperatorRightPlus
import vct.col.ast.ops.OperatorRightPlusOps
import vct.col.print._

trait OperatorRightPlusImpl[G] extends OperatorRightPlusOps[G] {
  this: OperatorRightPlus[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
