package vct.col.ast.expr.op.vec

import vct.col.ast.{MatrixCompare, TInt, TMatrix, Type}

trait MatrixCompareImpl[G] { this: MatrixCompare[G] =>
  override def t: Type[G] = TMatrix(TInt())
}