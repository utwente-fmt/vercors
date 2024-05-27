package vct.col.ast.expr.op.cmp

import vct.col.ast.{TInt, TVector, Type, VectorEq}
import vct.col.ast.ops.VectorEqOps
import vct.col.print._

trait VectorEqImpl[G] extends VectorEqOps[G] { this: VectorEq[G] =>
  override def t: Type[G] = TVector[G](getVectorType.size, TInt())

  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "==", right)

}
