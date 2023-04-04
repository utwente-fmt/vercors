package vct.col.ast.expr.ambiguous

import vct.col.ast.AmbiguousComputationalAnd
import vct.col.print._

trait AmbiguousComputationalAndImpl[G] { this: AmbiguousComputationalAnd[G] =>
  override def precedence: Int = Precedence.BIT_AND
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "&", right)
}