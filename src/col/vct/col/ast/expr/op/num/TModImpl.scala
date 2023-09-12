package vct.col.ast.expr.op.num

import vct.col.ast.TMod
import vct.col.print.{Ctx, Doc, Precedence}

trait TModImpl[G] { this: TMod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}