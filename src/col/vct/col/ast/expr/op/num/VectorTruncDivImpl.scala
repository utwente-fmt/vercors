package vct.col.ast.expr.op.num

import vct.col.ast.VectorTruncDiv
import vct.col.ast.ops.VectorTruncDivOps
import vct.col.print._

trait VectorTruncDivImpl[G] extends VectorTruncDivOps[G] { this: VectorTruncDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
