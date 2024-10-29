package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousEq
import vct.col.ast.ops.AmbiguousEqOps
import vct.col.print._

trait AmbiguousEqImpl[G] extends AmbiguousEqOps[G] {
  this: AmbiguousEq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "==", right)
}
