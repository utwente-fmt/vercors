package vct.col.ast.expr.op.num

import vct.col.ast.TDiv
import vct.col.print.{Ctx, Doc, Precedence}

trait TDivImpl[G] { this: TDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}