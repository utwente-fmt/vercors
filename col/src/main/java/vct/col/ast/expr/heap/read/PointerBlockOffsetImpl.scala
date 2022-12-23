package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerBlockOffset, TInt, Type}

trait PointerBlockOffsetImpl[G] { this: PointerBlockOffset[G] =>
  override def t: Type[G] = TInt()
}
