package vct.col.ast.expr.op.bool

import vct.col.ast.{ComputationalXor, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait ComputationalXorImpl[G] { this: ComputationalXor[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.BIT_XOR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "^", right)
}