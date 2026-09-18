package vct.col.ast.expr.op.cmp

import vct.col.ast.PointerEq
import vct.col.ast.ops.PointerEqOps
import vct.col.print.{Ctx, Doc, Precedence}

trait PointerEqImpl[G] extends PointerEqOps[G] {
  this: PointerEq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "==", right)
}
