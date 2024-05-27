package vct.col.ast.unsorted

import vct.col.ast.Operator
import vct.col.ast.ops.OperatorFamilyOps
import vct.col.print._

trait OperatorImpl[G] extends OperatorFamilyOps[G] {
  this: Operator[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
