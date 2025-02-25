package vct.col.ast.expr.op.bool

import vct.col.ast.{And, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.AndOps

trait AndImpl[G] extends AndOps[G] {
  this: And[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.AND - 5
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "&&", right)
}
