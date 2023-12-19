package vct.col.ast.expr.op.num

import vct.col.ast.Minus
import vct.col.print.{Ctx, Doc, Precedence}

trait MinusImpl[G] { this: Minus[G] =>
  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "-", right)
}