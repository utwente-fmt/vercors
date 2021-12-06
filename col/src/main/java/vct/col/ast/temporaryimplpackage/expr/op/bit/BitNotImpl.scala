package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitNot, TInt, Type}

trait BitNotImpl { this: BitNot =>
  override def t: Type = TInt()
}