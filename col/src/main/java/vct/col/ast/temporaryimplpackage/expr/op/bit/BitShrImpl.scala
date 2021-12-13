package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitShr, TInt, Type}

trait BitShrImpl[G] { this: BitShr[G] =>
  override def t: Type[G] = TInt()
}