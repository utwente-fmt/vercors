package vct.col.ast.expr.ambiguous

import vct.col.ast.`type`.TFloats
import vct.col.ast.{AmbiguousMinus, TFloat, TInt, TRational, Type}
import vct.col.typerules.{CoercionUtils, Types}

trait AmbiguousMinusImpl[G] { this: AmbiguousMinus[G] =>
  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined
  def isPointerOp: Boolean = CoercionUtils.getAnyPointerCoercion(left.t).isDefined

  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined

  override def t: Type[G] = {
    if(isSetOp || isBagOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isIntOp) TInt()
    else if(isPointerOp) left.t
    else if(TFloats.isFloatOp(left.t, right.t)) {
      TFloats.coerceToMax[G](left.t, right.t)
    }
    else TRational()
  }
}
