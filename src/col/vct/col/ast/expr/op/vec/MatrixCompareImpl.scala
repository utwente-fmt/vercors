package vct.col.ast.expr.op.vec

import vct.col.ast.{MatrixCompare, TInt, TMatrix, Type}
import vct.col.ast.ops.MatrixCompareOps

trait MatrixCompareImpl[G] extends MatrixCompareOps[G] {
  this: MatrixCompare[G] =>
  override def t: Type[G] = TMatrix(TInt())
}
