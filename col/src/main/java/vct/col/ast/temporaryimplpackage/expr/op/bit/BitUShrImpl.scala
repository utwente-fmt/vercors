package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitUShr, TInt, Type}

trait BitUShrImpl[G] { this: BitUShr[G] =>
  override def t: Type[G] = TInt()
}