package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{IntegerValue, TBoundedInt, Type}

trait IntegerValueImpl { this: IntegerValue =>
  override def t: Type = TBoundedInt(value, value + 1)
}
