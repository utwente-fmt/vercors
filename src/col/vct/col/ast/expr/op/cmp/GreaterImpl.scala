package vct.col.ast.expr.op.cmp

import vct.col.ast.Greater
import vct.col.print.{Ctx, Doc, Precedence}

trait GreaterImpl[G] { this: Greater[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">", right)
}
