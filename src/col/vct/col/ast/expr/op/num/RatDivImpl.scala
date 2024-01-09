package vct.col.ast.expr.op.num

import vct.col.ast.{RatDiv, TRational, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.RatDivOps

trait RatDivImpl[G] extends RatDivOps[G] { this: RatDiv[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "\\", right)
}