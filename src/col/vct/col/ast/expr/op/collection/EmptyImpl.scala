package vct.col.ast.expr.op.collection

import vct.col.ast.{Empty, TBool, Type}

trait EmptyImpl[G] { this: Empty[G] =>
  override def t: Type[G] = TBool()
}