package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{MatrixSum, Type}

trait MatrixSumImpl { this: MatrixSum =>
  override def t: Type = mat.t.asMatrix.get.element
}