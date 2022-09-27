package vct.col.ast.expr.op.bit

import vct.col.ast.{BitShl, TInt, Type}

trait BitShlImpl[G] { this: BitShl[G] =>
  override def t: Type[G] = TInt()
}