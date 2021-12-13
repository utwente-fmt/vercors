package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitXor, TInt, Type}

trait BitXorImpl[G] { this: BitXor[G] =>
  override def t: Type[G] = TInt()
}