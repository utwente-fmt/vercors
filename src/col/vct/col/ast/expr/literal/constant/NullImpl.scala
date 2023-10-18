package vct.col.ast.expr.literal.constant

import vct.col.ast.{Null, TNull, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait NullImpl[G] {
  this: Null[G] =>
  override def t: Type[G] = TNull()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("null")
}
