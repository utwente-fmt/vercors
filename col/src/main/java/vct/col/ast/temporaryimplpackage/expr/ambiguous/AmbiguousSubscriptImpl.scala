package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast.{AmbiguousSubscript, Type}
import vct.col.coerce.Coercion

trait AmbiguousSubscriptImpl[G] { this: AmbiguousSubscript[G] =>
  def isSeqOp: Boolean = Coercion.getAnySeqCoercion(collection.t).isDefined
  def isArrayOp: Boolean = Coercion.getAnyArrayCoercion(collection.t).isDefined
  def isPointerOp: Boolean = Coercion.getAnyPointerCoercion(collection.t).isDefined
  def isMapOp: Boolean = Coercion.getAnyMapCoercion(collection.t).isDefined

  override def t: Type[G] =
    if (isSeqOp) collection.t.asSeq.get.element
    else if (isArrayOp) collection.t.asArray.get.element
    else if (isPointerOp) collection.t.asPointer.get.element
    else collection.t.asMap.get.value
}