package vct.col.ast.expr.op.num

import vct.col.ast.TruncDiv
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.TruncDivOps

trait TruncDivImpl[G] extends TruncDivOps[G] { this: TruncDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}