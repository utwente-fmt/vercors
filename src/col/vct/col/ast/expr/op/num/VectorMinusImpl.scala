package vct.col.ast.expr.op.num

import vct.col.ast.VectorMinus
import vct.col.ast.ops.VectorMinusOps
import vct.col.print.{Ctx, Doc, Precedence}

trait VectorMinusImpl[G] extends VectorMinusOps[G] {
  this: VectorMinus[G] =>
  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "-", right)
}
