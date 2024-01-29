package vct.col.ast.expr.op.num

import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.{TVector, Type, VectorBinExpr}

trait VectorBinExprImpl[G] { this: VectorBinExpr[G] =>
  override def t: Type[G] = {
    val TVector(sizeL, elementTypeL) = left.t
    val TVector(sizeR, elementTypeR) = right.t
    val elementType = BinOperatorTypes.getNumericType(elementTypeL, elementTypeR, this.o)
    if(sizeL != sizeR) ???
    TVector(sizeL, elementType)
  }
}