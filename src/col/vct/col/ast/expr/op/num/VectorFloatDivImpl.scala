package vct.col.ast.expr.op.num

import vct.col.ast.VectorFloatDiv
import vct.col.ast.ops.VectorFloatDivOps
import vct.col.print._

trait VectorFloatDivImpl[G] extends VectorFloatDivOps[G] {
  this: VectorFloatDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
