package vct.col.ast.expr.context

import vct.col.ast.{TClass, ThisObject, Type}

trait ThisObjectImpl[G] { this: ThisObject[G] =>
  override def t: Type[G] = TClass(cls)
}