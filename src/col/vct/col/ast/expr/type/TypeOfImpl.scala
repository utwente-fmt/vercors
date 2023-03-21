package vct.col.ast.expr.`type`

import vct.col.ast.{TType, Type, TypeOf}

trait TypeOfImpl[G] { this: TypeOf[G] =>
  override def t: Type[G] = TType(expr.t)
}