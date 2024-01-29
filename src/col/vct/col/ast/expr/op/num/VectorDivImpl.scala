package vct.col.ast.expr.op.num

import vct.col.ast.VectorDiv
import vct.col.ast.ops.VectorDivOps
import vct.col.print.{Ctx, Doc, Precedence}

trait VectorDivImpl[G] extends VectorDivOps[G] { this: VectorDiv[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
