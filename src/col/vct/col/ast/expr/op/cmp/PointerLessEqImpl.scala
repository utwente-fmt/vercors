package vct.col.ast.expr.op.cmp

import vct.col.ast.PointerLessEq
import vct.col.ast.ops.PointerLessEqOps
import vct.col.print.{Ctx, Doc, Precedence}

trait PointerLessEqImpl[G] extends PointerLessEqOps[G] {
  this: PointerLessEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<=", right)
}
