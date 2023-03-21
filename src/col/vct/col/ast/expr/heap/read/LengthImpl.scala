package vct.col.ast.expr.heap.read

import vct.col.ast.{Length, TInt, Type}

trait LengthImpl[G] { this: Length[G] =>
  override def t: Type[G] = TInt()
}