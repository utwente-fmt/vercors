package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{BooleanValue, TBool, Type}

trait BooleanValueImpl[G] { this: BooleanValue[G] =>
  override def t: Type[G] = TBool()
}
