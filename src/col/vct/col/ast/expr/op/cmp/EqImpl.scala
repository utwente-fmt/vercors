package vct.col.ast.expr.op.cmp

import vct.col.ast.Eq
import vct.col.print.{Ctx, Doc, Precedence}

trait EqImpl[G] {
  this: Eq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "==", right)
}
