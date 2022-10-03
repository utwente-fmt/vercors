package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerAdd, Type}

trait PointerAddImpl[G] { this: PointerAdd[G] =>
  override def t: Type[G] = pointer.t
}