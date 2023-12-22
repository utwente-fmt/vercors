package vct.col.ast.expr.op.num

import vct.col.ast.Plus
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.PlusOps

trait PlusImpl[G] extends PlusOps[G] { this: Plus[G] =>
  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "+", right)
}