package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast.{AmbiguousMinus, TInt, TRational, Type}
import vct.col.coerce.CoercionUtils
import vct.col.util.Types

trait AmbiguousMinusImpl[G] { this: AmbiguousMinus[G] =>
  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined
  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined

  override def t: Type[G] = {
    if(isSetOp || isBagOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isIntOp) TInt()
    else TRational()
  }
}
