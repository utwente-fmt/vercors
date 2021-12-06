package vct.col.ast.temporaryimplpackage.expr.`type`

import vct.col.ast.{InstanceOf, TBool, Type}

trait InstanceOfImpl { this: InstanceOf =>
  override def t: Type = TBool()
}