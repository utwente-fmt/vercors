package vct.col.ast.expr.op.vec

import vct.col.ast.{Type, VectorSum}
import vct.col.ast.ops.VectorSumOps

trait VectorSumImpl[G] extends VectorSumOps[G] {
  this: VectorSum[G] =>
  override def t: Type[G] = vec.t.asSeq.get.element
}
