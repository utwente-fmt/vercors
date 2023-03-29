package vct.col.ast.expr.op.bool

import vct.col.ast.{And, TBool, Type}

trait AndImpl[G] { this: And[G] =>
  override def t: Type[G] = TBool()
}