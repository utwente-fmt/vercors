package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousLess
import vct.col.print.{Ctx, Doc, Precedence}

trait AmbiguousLessImpl[G] { this: AmbiguousLess[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<", right)
}