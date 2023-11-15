package vct.col.ast.expr.op.num

import vct.col.ast.TruncDiv
import vct.col.print.{Ctx, Doc, Precedence}

trait TruncDivImpl[G] { this: TruncDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}