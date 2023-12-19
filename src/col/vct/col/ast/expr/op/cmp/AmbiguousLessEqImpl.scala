package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousLessEq
import vct.col.print.{Ctx, Doc, Precedence}

trait AmbiguousLessEqImpl[G] { this: AmbiguousLessEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<=", right)
}