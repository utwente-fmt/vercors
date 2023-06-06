package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousGreaterEq
import vct.col.print.{Ctx, Doc, Precedence}

trait AmbiguousGreaterEqImpl[G] { this: AmbiguousGreaterEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">=", right)
}