package vct.col.ast.expr.ambiguous

import vct.col.ast.AmbiguousComputationalAnd
import vct.col.print._
import vct.col.ast.ops.AmbiguousComputationalAndOps

trait AmbiguousComputationalAndImpl[G] extends AmbiguousComputationalAndOps[G] {
  this: AmbiguousComputationalAnd[G] =>
  override def precedence: Int = Precedence.BIT_AND
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "&", right)
}
