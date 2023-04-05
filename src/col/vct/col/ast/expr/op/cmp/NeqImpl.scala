package vct.col.ast.expr.op.cmp

import vct.col.ast.Neq
import vct.col.print.{Ctx, Doc, Precedence}

trait NeqImpl[G] { this: Neq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "!=", right)

}