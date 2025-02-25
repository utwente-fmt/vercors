package vct.col.ast.expr.op.cmp

import vct.col.ast.SubBagEq
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.SubBagEqOps

trait SubBagEqImpl[G] extends SubBagEqOps[G] {
  this: SubBagEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<=", right)
}
