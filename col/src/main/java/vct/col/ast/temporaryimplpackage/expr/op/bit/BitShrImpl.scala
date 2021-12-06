package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitShr, TInt, Type}

trait BitShrImpl { this: BitShr =>
  override def t: Type = TInt()
}