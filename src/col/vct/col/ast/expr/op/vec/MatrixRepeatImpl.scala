package vct.col.ast.expr.op.vec

import vct.col.ast.{MatrixRepeat, TMatrix, Type}
import vct.col.ast.ops.MatrixRepeatOps

trait MatrixRepeatImpl[G] extends MatrixRepeatOps[G] {
  this: MatrixRepeat[G] =>
  override def t: Type[G] = TMatrix(e.t)
}
