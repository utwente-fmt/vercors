package vct.col.ast.expr.op.cmp

import vct.col.ast.GreaterEq
import vct.col.print.{Ctx, Doc, Precedence}

trait GreaterEqImpl[G] {
  this: GreaterEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">=", right)
}
