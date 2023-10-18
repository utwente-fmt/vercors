package vct.col.ast.expr.op.cmp

import vct.col.ast.SubBagEq
import vct.col.print.{Ctx, Doc, Precedence}

trait SubBagEqImpl[G] {
  this: SubBagEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<=", right)
}
