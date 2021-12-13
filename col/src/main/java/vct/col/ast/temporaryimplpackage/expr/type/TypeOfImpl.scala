package vct.col.ast.temporaryimplpackage.expr.`type`

import vct.col.ast.{TType, Type, TypeOf}

trait TypeOfImpl[G] { this: TypeOf[G] =>
  override def t: Type[G] = TType(expr.t)
}