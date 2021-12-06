package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{Length, TInt, Type}

trait LengthImpl { this: Length =>
  override def t: Type = TInt()
}