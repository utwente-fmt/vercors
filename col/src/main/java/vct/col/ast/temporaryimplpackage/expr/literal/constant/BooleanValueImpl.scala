package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{BooleanValue, TBool, Type}

trait BooleanValueImpl { this: BooleanValue =>
  override def t: Type = TBool()
}
