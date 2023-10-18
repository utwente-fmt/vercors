package vct.col.ast.expr.op.cmp

import vct.col.ast.SubSetEq
import vct.col.print.{Ctx, Doc, Precedence}

trait SubSetEqImpl[G] {
  this: SubSetEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<=", right)
}
