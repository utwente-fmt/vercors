package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Empty, TBool, Type}

trait EmptyImpl { this: Empty =>
  override def t: Type = TBool()
}