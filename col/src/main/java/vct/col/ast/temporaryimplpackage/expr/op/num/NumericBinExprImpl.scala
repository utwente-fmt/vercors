package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{NumericBinExpr, TInt, TRational, Type}
import vct.col.coerce.Coercion

trait NumericBinExprImpl { this: NumericBinExpr =>
  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type = if(isIntOp) TInt() else TRational()
}