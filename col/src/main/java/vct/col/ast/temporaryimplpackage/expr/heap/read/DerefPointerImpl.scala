package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{DerefPointer, Type}

trait DerefPointerImpl { this: DerefPointer =>
  override def t: Type = pointer.t.asPointer.get.element
}