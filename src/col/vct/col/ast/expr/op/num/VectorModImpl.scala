package vct.col.ast.expr.op.num

import vct.col.ast.VectorMod
import vct.col.ast.ops.VectorModOps
import vct.col.print._

trait VectorModImpl[G] extends VectorModOps[G] { this: VectorMod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}
