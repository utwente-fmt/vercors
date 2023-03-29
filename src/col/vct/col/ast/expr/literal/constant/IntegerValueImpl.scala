package vct.col.ast.expr.literal.constant

import vct.col.ast.{IntegerValue, TBoundedInt, Type}

trait IntegerValueImpl[G] { this: IntegerValue[G] =>
  override def t: Type[G] = TBoundedInt(value, value + 1)
}
