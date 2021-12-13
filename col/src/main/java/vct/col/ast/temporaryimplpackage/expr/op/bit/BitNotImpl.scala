package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitNot, TInt, Type}

trait BitNotImpl[G] { this: BitNot[G] =>
  override def t: Type[G] = TInt()
}