package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitAnd, TInt, Type}

trait BitAndImpl { this: BitAnd =>
  override def t: Type = TInt()
}