package vct.col.ast.expr.ambiguous

import vct.col.ast._
import vct.col.typerules.CoercionUtils

trait AmbiguousMultImpl[G] { this: AmbiguousMult[G] =>
  def isProcessOp: Boolean = CoercionUtils.getCoercion(left.t, TProcess()).isDefined
  def isIntOp: Boolean = CoercionUtils.getCoercion(left.t, TInt()).isDefined && CoercionUtils.getCoercion(right.t, TInt()).isDefined
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined
  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined

  override def t: Type[G] = if(isProcessOp) TProcess() else (if(isIntOp) TInt() else TRational())
}