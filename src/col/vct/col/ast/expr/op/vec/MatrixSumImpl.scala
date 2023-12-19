package vct.col.ast.expr.op.vec

import vct.col.ast.{MatrixSum, Type}

trait MatrixSumImpl[G] { this: MatrixSum[G] =>
  override def t: Type[G] = mat.t.asMatrix.get.element
}