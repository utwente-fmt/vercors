package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast._
import vct.col.coerce.CoercionUtils
import vct.col.util.Types

trait AmbiguousPlusImpl[G] { this: AmbiguousPlus[G] =>
  def isProcessOp: Boolean = CoercionUtils.getCoercion(left.t, TProcess()).isDefined
  def isSeqOp: Boolean = CoercionUtils.getAnySeqCoercion(left.t).isDefined
  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined
  def isPointerOp: Boolean = CoercionUtils.getAnyPointerCoercion(left.t).isDefined
  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined
  def isFloatOp: Boolean =
    CoercionUtils.getCoercion(left.t, TFloat()).isDefined &&
      CoercionUtils.getCoercion(right.t, TFloat()).isDefined

  override def t: Type[G] =
    if(isProcessOp) TProcess()
    else if(isSeqOp || isBagOp || isSetOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isPointerOp) left.t
    else if(isIntOp) TInt()
    else if(isFloatOp) TFloat()
    else TRational()
}