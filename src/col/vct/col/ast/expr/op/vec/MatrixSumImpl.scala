package vct.col.ast.expr.op.vec

import vct.col.ast.{MatrixSum, Type}
import vct.col.ast.ops.MatrixSumOps

trait MatrixSumImpl[G] extends MatrixSumOps[G] {
  this: MatrixSum[G] =>
  override def t: Type[G] = mat.t.asMatrix.get.element
}
