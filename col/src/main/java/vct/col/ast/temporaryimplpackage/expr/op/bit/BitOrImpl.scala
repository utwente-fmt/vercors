package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitOr, TInt, Type}

trait BitOrImpl[G] { this: BitOr[G] =>
  override def t: Type[G] = TInt()
}