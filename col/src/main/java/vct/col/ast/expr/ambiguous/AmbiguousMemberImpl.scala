package vct.col.ast.expr.ambiguous

import vct.col.ast.{AmbiguousMember, TBool, TInt, Type}
import vct.col.typerules.CoercionUtils

trait AmbiguousMemberImpl[G] { this: AmbiguousMember[G] =>
  def isSeqOp: Boolean = CoercionUtils.getAnySeqCoercion(xs.t).isDefined
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(xs.t).isDefined
  def isMapOp: Boolean = CoercionUtils.getAnyMapCoercion(xs.t).isDefined
  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(xs.t).isDefined

  def collectionElementType: Type[G] =
    if(isSeqOp) xs.t.asSeq.get.element
    else if(isSetOp) xs.t.asSet.get.element
    else if(isBagOp) xs.t.asBag.get.element
    else xs.t.asMap.get.key

  override lazy val t: Type[G] = if(isBagOp) TInt() else TBool()
}