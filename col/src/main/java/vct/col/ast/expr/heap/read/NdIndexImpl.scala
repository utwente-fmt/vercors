package vct.col.ast.expr.heap.read

import vct.col.ast.{NdIndex, TInt}

trait NdIndexImpl[G] { this: NdIndex[G] =>
  override def t: TInt[G] = TInt()
}
