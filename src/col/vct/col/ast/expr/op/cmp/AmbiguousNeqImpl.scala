package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousNeq
import vct.col.ast.ops.AmbiguousNeqOps
import vct.col.print._

trait AmbiguousNeqImpl[G] extends AmbiguousNeqOps[G] {
  this: AmbiguousNeq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "!=", right)
}
