package vct.col.ast.expr.op.num

import vct.col.ast.Mult
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.MultOps

trait MultImpl[G] extends MultOps[G] {
  this: Mult[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "*", right)
}
