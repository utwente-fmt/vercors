package vct.col.ast.temporaryimplpackage.expr.`type`

import vct.col.ast.{TType, Type, TypeOf}

trait TypeOfImpl { this: TypeOf =>
  override def t: Type = TType(expr.t)
}