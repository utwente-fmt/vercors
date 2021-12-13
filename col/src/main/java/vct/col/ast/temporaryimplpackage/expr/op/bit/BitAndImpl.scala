package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitAnd, TInt, Type}

trait BitAndImpl[G] { this: BitAnd[G] =>
  override def t: Type[G] = TInt()
}