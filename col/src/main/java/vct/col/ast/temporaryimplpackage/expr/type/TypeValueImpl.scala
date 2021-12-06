package vct.col.ast.temporaryimplpackage.expr.`type`

import vct.col.ast.{TType, Type, TypeValue}

trait TypeValueImpl { this: TypeValue =>
  override def t: Type = TType(value)
}