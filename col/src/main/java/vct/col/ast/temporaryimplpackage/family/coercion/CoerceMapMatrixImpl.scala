package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceMapMatrix, TMatrix}

trait CoerceMapMatrixImpl[G] { this: CoerceMapMatrix[G] => 
  override def target: TMatrix[G] = TMatrix(targetMatrixElement)
}
