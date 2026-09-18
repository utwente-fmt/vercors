package vct.col.ast.expr.op.cmp

import vct.col.ast.PointerGreaterEq
import vct.col.ast.ops.PointerGreaterEqOps
import vct.col.print.{Ctx, Doc, Precedence}

trait PointerGreaterEqImpl[G] extends PointerGreaterEqOps[G] {
  this: PointerGreaterEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">=", right)
}
