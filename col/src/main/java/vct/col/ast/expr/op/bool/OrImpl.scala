package vct.col.ast.expr.op.bool

import vct.col.ast.{Or, TBool, Type}

trait OrImpl[G] { this: Or[G] =>
  override def t: Type[G] = TBool()
}