package vct.col.ast.expr.op.num

import vct.col.ast.VectorTruncMod
import vct.col.ast.ops.VectorTruncModOps
import vct.col.print._

trait VectorTruncModImpl[G] extends VectorTruncModOps[G] { this: VectorTruncMod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}
