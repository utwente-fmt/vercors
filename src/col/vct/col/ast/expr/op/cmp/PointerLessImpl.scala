package vct.col.ast.expr.op.cmp

import vct.col.ast.PointerLess
import vct.col.ast.ops.PointerLessOps
import vct.col.print.{Ctx, Doc, Precedence}

trait PointerLessImpl[G] extends PointerLessOps[G] {
  this: PointerLess[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<", right)
}
