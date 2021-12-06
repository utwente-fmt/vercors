package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitUShr, TInt, Type}

trait BitUShrImpl { this: BitUShr =>
  override def t: Type = TInt()
}