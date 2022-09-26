package vct.col.ast.expr.ambiguous

import vct.col.ast.{AmbiguousSubscript, Type}
import vct.col.coerce.CoercionUtils

trait AmbiguousSubscriptImpl[G] { this: AmbiguousSubscript[G] =>
  def isSeqOp: Boolean = CoercionUtils.getAnySeqCoercion(collection.t).isDefined
  def isArrayOp: Boolean = CoercionUtils.getAnyArrayCoercion(collection.t).isDefined
  def isPointerOp: Boolean = CoercionUtils.getAnyPointerCoercion(collection.t).isDefined
  def isMapOp: Boolean = CoercionUtils.getAnyMapCoercion(collection.t).isDefined

  override def t: Type[G] =
    if (isSeqOp) collection.t.asSeq.get.element
    else if (isArrayOp) collection.t.asArray.get.element
    else if (isPointerOp) collection.t.asPointer.get.element
    else collection.t.asMap.get.value
}