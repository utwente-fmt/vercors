package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{PointerSubscript, Type}

trait PointerSubscriptImpl { this: PointerSubscript =>
  override def t: Type = pointer.t.asPointer.get.element
}