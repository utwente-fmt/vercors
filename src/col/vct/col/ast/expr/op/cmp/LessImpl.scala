package vct.col.ast.expr.op.cmp

import vct.col.ast.Less
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.LessOps

trait LessImpl[G] extends LessOps[G] {
  this: Less[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<", right)
}
