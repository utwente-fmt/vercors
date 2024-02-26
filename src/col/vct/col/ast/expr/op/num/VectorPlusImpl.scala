package vct.col.ast.expr.op.num

import vct.col.ast.VectorPlus
import vct.col.ast.ops.VectorPlusOps
import vct.col.print._

trait VectorPlusImpl[G] extends VectorPlusOps[G] { this: VectorPlus[G] =>
  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "+", right)
}
