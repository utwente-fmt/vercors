package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{NumericBinExpr, TInt, TRational, TFloat, Type}
import vct.col.coerce.CoercionUtils

trait NumericBinExprImpl[G] { this: NumericBinExpr[G] =>
  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined
  def isFloat: Boolean =
    CoercionUtils.getCoercion(left.t, TFloat()).isDefined &&
      CoercionUtils.getCoercion(right.t, TFloat()).isDefined

  override def t: Type[G] = {
    if (isIntOp) TInt()
    else if (isFloat) TFloat()
    else TRational()
  }
}