package vct.col.ast.expr.op.num

import vct.col.ast.VectorFloorDiv
import vct.col.ast.ops.VectorFloorDivOps
import vct.col.print._

trait VectorFloorDivImpl[G] extends VectorFloorDivOps[G] { this: VectorFloorDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
