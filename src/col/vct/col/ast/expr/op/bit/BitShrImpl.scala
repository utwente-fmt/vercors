package vct.col.ast.expr.op.bit

import vct.col.ast.{BitShr, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait BitShrImpl[G] { this: BitShr[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.SHIFT
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">>", right)
}