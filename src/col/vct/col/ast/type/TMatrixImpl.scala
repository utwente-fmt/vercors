package vct.col.ast.`type`

import vct.col.ast.{TMatrix, Type}
import vct.col.ast.ops.TMatrixOps

trait TMatrixImpl[G] extends TMatrixOps[G] {
  this: TMatrix[G] =>
  override def composingTypes: Seq[Type[G]] = Seq(element)
}
