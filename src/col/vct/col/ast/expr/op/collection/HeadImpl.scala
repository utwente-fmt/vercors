package vct.col.ast.expr.op.collection

import vct.col.ast.{Head, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.HeadOps

trait HeadImpl[G] extends HeadOps[G] {
  this: Head[G] =>
  override def t: Type[G] = xs.t.asSeq.get.element

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(xs) <> ".head"
}
