package vct.col.ast.expr.op.cmp

import vct.col.ast.Neq
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.NeqOps

trait NeqImpl[G] extends NeqOps[G] {
  this: Neq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "!=", right)

}
