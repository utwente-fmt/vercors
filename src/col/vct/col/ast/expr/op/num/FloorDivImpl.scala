package vct.col.ast.expr.op.num

import vct.col.ast.{FloorDiv, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait FloorDivImpl[G] {
  this: FloorDiv[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
