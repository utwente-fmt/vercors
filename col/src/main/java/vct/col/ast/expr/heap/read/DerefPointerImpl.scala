package vct.col.ast.expr.heap.read

import vct.col.ast.{DerefPointer, Type}

trait DerefPointerImpl[G] { this: DerefPointer[G] =>
  override def t: Type[G] = pointer.t.asPointer.get.element
}