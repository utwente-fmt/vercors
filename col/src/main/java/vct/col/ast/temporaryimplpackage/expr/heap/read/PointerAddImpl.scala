package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{PointerAdd, Type}

trait PointerAddImpl { this: PointerAdd =>
  override def t: Type = pointer.t
}