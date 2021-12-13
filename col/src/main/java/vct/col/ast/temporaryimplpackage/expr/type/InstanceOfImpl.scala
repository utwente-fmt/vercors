package vct.col.ast.temporaryimplpackage.expr.`type`

import vct.col.ast.{InstanceOf, TBool, Type}

trait InstanceOfImpl[G] { this: InstanceOf[G] =>
  override def t: Type[G] = TBool()
}