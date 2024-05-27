package vct.col.ast.expr.op.num

import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.{TVector, Type, VectorBinExpr}

trait VectorBinExprImpl[G] { this: VectorBinExpr[G] =>
  override def t: Type[G] = getVectorType
}