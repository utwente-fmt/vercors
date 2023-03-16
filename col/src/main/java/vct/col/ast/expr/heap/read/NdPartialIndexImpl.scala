package vct.col.ast.expr.heap.read

import vct.col.ast.{NdPartialIndex, TBool}

trait NdPartialIndexImpl[G] { this: NdPartialIndex[G] =>
  override def t: TBool[G] = TBool()
}
