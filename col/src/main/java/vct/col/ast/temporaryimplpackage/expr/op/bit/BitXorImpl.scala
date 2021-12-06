package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitXor, TInt, Type}

trait BitXorImpl { this: BitXor =>
  override def t: Type = TInt()
}