package vct.col.ast.`type`

import vct.col.ast.TMatrix
import vct.col.ast.ops.TMatrixOps

trait TMatrixImpl[G] extends TMatrixOps[G] { this: TMatrix[G] =>

}