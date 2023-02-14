package vct.col.ast.expr.heap.read

import vct.col.ast.{NdLinearIndex, TBool}

trait NdLinearIndexImpl[G] { this: NdLinearIndex[G] =>
  override def t: TBool[G] = TBool()
}
