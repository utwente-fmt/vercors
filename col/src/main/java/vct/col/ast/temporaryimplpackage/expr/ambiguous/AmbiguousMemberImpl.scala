package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast.{AmbiguousMember, TBool, TInt, Type}
import vct.col.coerce.Coercion

trait AmbiguousMemberImpl { this: AmbiguousMember =>
  def isSeqOp: Boolean = Coercion.getAnySeqCoercion(xs.t).isDefined
  def isSetOp: Boolean = Coercion.getAnySetCoercion(xs.t).isDefined
  def isMapOp: Boolean = Coercion.getAnyMapCoercion(xs.t).isDefined
  def isBagOp: Boolean = Coercion.getAnyBagCoercion(xs.t).isDefined

  def collectionElementType: Type =
    if(isSeqOp) xs.t.asSeq.get.element
    else if(isSetOp) xs.t.asSet.get.element
    else if(isBagOp) xs.t.asBag.get.element
    else xs.t.asMap.get.key

  override def t: Type = if(isBagOp) TInt() else TBool()
}