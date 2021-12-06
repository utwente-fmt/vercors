package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{MatrixRepeat, TMatrix, Type}

trait MatrixRepeatImpl { this: MatrixRepeat =>
  override def t: Type = TMatrix(e.t)
}