package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{Type, VectorSum}

trait VectorSumImpl { this: VectorSum =>
  override def t: Type = vec.t.asSeq.get.element
}