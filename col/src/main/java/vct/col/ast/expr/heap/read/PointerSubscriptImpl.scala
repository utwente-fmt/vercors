package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerSubscript, Type}

trait PointerSubscriptImpl[G] { this: PointerSubscript[G] =>
  override def t: Type[G] = pointer.t.asPointer.get.element
}