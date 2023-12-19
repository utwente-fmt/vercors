package vct.col.ast.expr.op.collection

import vct.col.ast.{Head, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait HeadImpl[G] { this: Head[G] =>
  override def t: Type[G] = xs.t.asSeq.get.element

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(xs) <> ".head"
}