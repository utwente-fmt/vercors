package vct.col.ast.expr.op.cmp

import vct.col.ast.PointerNeq
import vct.col.ast.ops.PointerNeqOps
import vct.col.print.{Ctx, Doc, Precedence}

trait PointerNeqImpl[G] extends PointerNeqOps[G] {
  this: PointerNeq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "!=", right)

}
