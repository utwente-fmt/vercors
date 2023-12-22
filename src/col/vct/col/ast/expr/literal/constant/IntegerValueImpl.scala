package vct.col.ast.expr.literal.constant

import vct.col.ast.{IntegerValue, TBoundedInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.IntegerValueOps

trait IntegerValueImpl[G] extends IntegerValueOps[G] {
  this: IntegerValue[G] =>
  override def t: Type[G] = TBoundedInt(value, value + 1)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(value.toString())
}
