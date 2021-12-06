package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitOr, TInt, Type}

trait BitOrImpl { this: BitOr =>
  override def t: Type = TInt()
}