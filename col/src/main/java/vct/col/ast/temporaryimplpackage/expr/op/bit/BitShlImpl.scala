package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitShl, TInt, Type}

trait BitShlImpl { this: BitShl =>
  override def t: Type = TInt()
}