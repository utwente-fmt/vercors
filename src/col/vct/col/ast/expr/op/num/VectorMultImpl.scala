package vct.col.ast.expr.op.num

import vct.col.ast.VectorMult
import vct.col.ast.ops.VectorMultOps
import vct.col.print.{Ctx, Doc, Precedence}

trait VectorMultImpl[G] extends VectorMultOps[G] { this: VectorMult[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "*", right)
}
