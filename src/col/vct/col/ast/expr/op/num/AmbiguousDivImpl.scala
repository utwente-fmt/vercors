package vct.col.ast.expr.op.num

import vct.col.ast.AmbiguousDiv
import vct.col.ast.ops.AmbiguousDivOps
import vct.col.print._

trait AmbiguousDivImpl[G] extends AmbiguousDivOps[G] { this: AmbiguousDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
