package vct.col.ast.expr.op.bit

import vct.col.ast.{BitUShr, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait BitUShrImpl[G] {
  this: BitUShr[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.SHIFT
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">>>", right)
}
