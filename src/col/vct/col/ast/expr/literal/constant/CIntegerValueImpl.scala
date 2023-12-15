package vct.col.ast.expr.literal.constant

import vct.col.ast.{CIntegerValue, TCInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait CIntegerValueImpl[G] {
  this: CIntegerValue[G] =>
  override def t: Type[G] = TCInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(value.toString())
}
