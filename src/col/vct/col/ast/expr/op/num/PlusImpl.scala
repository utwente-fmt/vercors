package vct.col.ast.expr.op.num

import vct.col.ast.Plus
import vct.col.print.{Ctx, Doc, Precedence}

trait PlusImpl[G] {
  this: Plus[G] =>
  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "+", right)
}
