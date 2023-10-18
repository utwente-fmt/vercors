package vct.col.ast.expr.ambiguous

import vct.col.ast.AmbiguousComputationalOr
import vct.col.print.{Ctx, Doc, Precedence}

trait AmbiguousComputationalOrImpl[G] {
  this: AmbiguousComputationalOr[G] =>
  override def precedence: Int = Precedence.BIT_OR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "|", right)
}
