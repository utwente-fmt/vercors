package vct.col.ast.expr.op.bit

import vct.col.ast.{BitXor, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait BitXorImpl[G] { this: BitXor[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.BIT_XOR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "^", right)
}