package vct.col.ast.expr.literal.constant

import vct.col.ast.ops.CheckedIntegerValueOps
import vct.col.ast.{CheckedIntegerValue, TBoundedInt, TCheckedInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait CheckedIntegerValueImpl[G] extends CheckedIntegerValueOps[G] {
  this: CheckedIntegerValue[G] =>
  override def t: Type[G] = TCheckedInt(gte, lt)(blame)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(value.toString())
}
