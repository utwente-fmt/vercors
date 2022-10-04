package vct.col.ast.expr.op.num

import vct.col.ast.`type`.TFloats
import vct.col.ast.{NumericBinExpr, TFloat, TInt, TRational, Type}
import vct.col.typerules.CoercionUtils

trait NumericBinExprImpl[G] { this: NumericBinExpr[G] =>
  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined

  def isFloatOp: Boolean = CoercionUtils.getCoercion(left.t, TFloats.max).isDefined &&
    CoercionUtils.getCoercion(right.t, TFloats.max).isDefined

  override def t: Type[G] =
    if(isIntOp) TInt()
    else if(isFloatOp)
      TFloats.max[G](left.t.asInstanceOf[TFloat[G]], right.t.asInstanceOf[TFloat[G]])
    else TRational()
}