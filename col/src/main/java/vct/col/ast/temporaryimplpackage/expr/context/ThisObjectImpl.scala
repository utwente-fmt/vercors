package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.{TClass, ThisObject, Type}

trait ThisObjectImpl { this: ThisObject =>
  override def t: Type = TClass(cls)
}