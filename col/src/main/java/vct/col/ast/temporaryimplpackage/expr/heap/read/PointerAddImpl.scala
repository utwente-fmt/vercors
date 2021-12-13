package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{PointerAdd, Type}

trait PointerAddImpl[G] { this: PointerAdd[G] =>
  override def t: Type[G] = pointer.t
}