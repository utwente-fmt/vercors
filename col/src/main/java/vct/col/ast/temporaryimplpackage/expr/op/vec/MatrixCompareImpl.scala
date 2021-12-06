package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{MatrixCompare, TInt, TMatrix, Type}

trait MatrixCompareImpl { this: MatrixCompare =>
  override def t: Type = TMatrix(TInt())
}