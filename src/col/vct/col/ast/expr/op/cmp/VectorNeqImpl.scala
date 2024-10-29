package vct.col.ast.expr.op.cmp

import vct.col.ast.{TInt, TVector, Type, VectorNeq}
import vct.col.ast.ops.VectorNeqOps
import vct.col.print._

trait VectorNeqImpl[G] extends VectorNeqOps[G] {
  this: VectorNeq[G] =>
  override def t: Type[G] = TVector[G](getVectorType.size, TInt())

  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "!=", right)
}
