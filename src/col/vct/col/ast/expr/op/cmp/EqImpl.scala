package vct.col.ast.expr.op.cmp

import vct.col.ast.Eq
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.EqOps

trait EqImpl[G] extends EqOps[G] {
  this: Eq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "==", right)
}
