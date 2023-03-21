package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerBlockLength, TInt, Type}

trait PointerBlockLengthImpl[G] { this: PointerBlockLength[G] =>
  override def t: Type[G] = TInt()
}
