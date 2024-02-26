package vct.col.ast.expr.op.num

import vct.col.ast.AmbiguousTruncDiv
import vct.col.ast.ops.AmbiguousTruncDivOps
import vct.col.print._

trait AmbiguousTruncDivImpl[G] extends AmbiguousTruncDivOps[G] { this: AmbiguousTruncDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
