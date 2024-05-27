package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousLess
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.AmbiguousLessOps

trait AmbiguousLessImpl[G] extends AmbiguousLessOps[G] {
  this: AmbiguousLess[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<", right)
}
