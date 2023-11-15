package vct.col.ast.expr.op.num

import vct.col.ast.TruncMod
import vct.col.print.{Ctx, Doc, Precedence}

trait TruncModImpl[G] { this: TruncMod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}