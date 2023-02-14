package vct.col.ast.expr.heap.read

import vct.col.ast.{NdLength, TInt}

trait NdLengthImpl[G] { this: NdLength[G] =>
  override def t: TInt[G] = TInt()
}
