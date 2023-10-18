package vct.col.ast.expr.op.bool

import vct.col.ast.{ComputationalOr, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait ComputationalOrImpl[G] {
  this: ComputationalOr[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.BIT_OR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "|", right)
}
