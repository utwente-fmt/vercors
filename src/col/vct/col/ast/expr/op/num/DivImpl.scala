package vct.col.ast.expr.op.num

import vct.col.ast.{Div, TRational, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait DivImpl[G] { this: Div[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "\\", right)
}