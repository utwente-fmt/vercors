package vct.col.ast.expr.op.num

import vct.col.ast.{FloorDiv, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.FloorDivOps

trait FloorDivImpl[G] extends FloorDivOps[G] {
  this: FloorDiv[G] =>
  override def t: Type[G] = TInt()
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
