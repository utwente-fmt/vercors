package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Empty, TBool, Type}

trait EmptyImpl[G] { this: Empty[G] =>
  override def t: Type[G] = TBool()
}