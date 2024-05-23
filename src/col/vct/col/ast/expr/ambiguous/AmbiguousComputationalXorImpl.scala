package vct.col.ast.expr.ambiguous

import vct.col.ast.AmbiguousComputationalXor
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.AmbiguousComputationalXorOps

trait AmbiguousComputationalXorImpl[G] extends AmbiguousComputationalXorOps[G] {
  this: AmbiguousComputationalXor[G] =>
  override def precedence: Int = Precedence.BIT_XOR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "^", right)
}
