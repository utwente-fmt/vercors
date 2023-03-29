package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapMatrix, TMatrix}

trait CoerceMapMatrixImpl[G] { this: CoerceMapMatrix[G] => 
  override def target: TMatrix[G] = TMatrix(targetMatrixElement)
}
