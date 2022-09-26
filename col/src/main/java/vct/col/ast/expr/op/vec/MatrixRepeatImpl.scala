package vct.col.ast.expr.op.vec

import vct.col.ast.{MatrixRepeat, TMatrix, Type}

trait MatrixRepeatImpl[G] { this: MatrixRepeat[G] =>
  override def t: Type[G] = TMatrix(e.t)
}