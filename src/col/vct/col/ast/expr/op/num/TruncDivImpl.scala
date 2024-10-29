package vct.col.ast.expr.op.num

import vct.col.ast.{TInt, TruncDiv, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.TruncDivOps

trait TruncDivImpl[G] extends TruncDivOps[G] {
  this: TruncDiv[G] =>
  override def t: Type[G] = TInt()
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
