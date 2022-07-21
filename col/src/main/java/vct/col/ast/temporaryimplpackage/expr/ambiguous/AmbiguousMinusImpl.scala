package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast.{AmbiguousMinus, TInt, TFloat, TRational, Type}
import vct.col.coerce.CoercionUtils
import vct.col.util.Types

trait AmbiguousMinusImpl[G] { this: AmbiguousMinus[G] =>
  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined
  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined
  def isFloatOp: Boolean =
    CoercionUtils.getCoercion(left.t, TFloat()).isDefined &&
      CoercionUtils.getCoercion(right.t, TFloat()).isDefined

  override def t: Type[G] = {
    if(isSetOp || isBagOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isIntOp) TInt()
    else if(isFloatOp) TFloat()
    else TRational()
  }
}
