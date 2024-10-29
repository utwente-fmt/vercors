package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapMatrix, TMatrix}
import vct.col.ast.ops.CoerceMapMatrixOps

trait CoerceMapMatrixImpl[G] extends CoerceMapMatrixOps[G] {
  this: CoerceMapMatrix[G] =>
  override def target: TMatrix[G] = TMatrix(targetMatrixElement)
}
