package vct.col.ast.expr.op.bool

import vct.col.ast.{And, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait AndImpl[G] {
  this: And[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.AND
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "&&", right)
}
