package vct.col.ast.expr.op.cmp

import vct.col.ast.GreaterEq
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.GreaterEqOps

trait GreaterEqImpl[G] extends GreaterEqOps[G] {
  this: GreaterEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">=", right)
}
