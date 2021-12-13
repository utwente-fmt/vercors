package vct.col.ast.temporaryimplpackage.expr.`type`

import vct.col.ast.{TType, Type, TypeValue}

trait TypeValueImpl[G] { this: TypeValue[G] =>
  override def t: Type[G] = TType(value)
}