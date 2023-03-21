package vct.col.ast.expr.op.num

import vct.col.ast.`type`.TFloats
import vct.col.ast.{NumericBinExpr, TFloat, TInt, TRational, Type}
import vct.col.typerules.CoercionUtils

trait NumericBinExprImpl[G] { this: NumericBinExpr[G] =>
  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined

  override def t: Type[G] =
    if(isIntOp) TInt()
    else if(TFloats.isFloatOp(left.t, right.t))
      TFloats.coerceToMax[G](left.t, right.t)
    else TRational()
}