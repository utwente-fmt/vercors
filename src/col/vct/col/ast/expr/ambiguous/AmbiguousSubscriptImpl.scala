package vct.col.ast.expr.ambiguous

import vct.col.ast.{AmbiguousSubscript, Type}
import vct.col.typerules.CoercionUtils
import vct.result.VerificationError.Unreachable

trait AmbiguousSubscriptImpl[G] { this: AmbiguousSubscript[G] =>
  def isSeqOp: Boolean = CoercionUtils.getAnySeqCoercion(collection.t).isDefined
  def isArrayOp: Boolean = CoercionUtils.getAnyArrayCoercion(collection.t).isDefined
  def isCArrayOp: Boolean = CoercionUtils.getAnyCArrayCoercion(collection.t).isDefined
  def isPointerOp: Boolean = CoercionUtils.getAnyPointerCoercion(collection.t).isDefined
  def isMapOp: Boolean = CoercionUtils.getAnyMapCoercion(collection.t).isDefined

  override lazy val t: Type[G] =
    if (isSeqOp) collection.t.asSeq.get.element
    else if (isArrayOp) collection.t.asArray.get.element
    else if (isCArrayOp) collection.t.asCArray.get.innerType
    else if (isPointerOp) collection.t.asPointer.get.element
    else if (isMapOp) collection.t.asMap.get.value
    else throw Unreachable(s"Trying to subscript ($this) a non subscriptable variable with type ${collection.t}")
}