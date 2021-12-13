package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{Type, VectorSum}

trait VectorSumImpl[G] { this: VectorSum[G] =>
  override def t: Type[G] = vec.t.asSeq.get.element
}