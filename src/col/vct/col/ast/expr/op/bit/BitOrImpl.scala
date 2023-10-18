package vct.col.ast.expr.op.bit

import vct.col.ast.{BitOr, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait BitOrImpl[G] {
  this: BitOr[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.BIT_OR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "|", right)
}
