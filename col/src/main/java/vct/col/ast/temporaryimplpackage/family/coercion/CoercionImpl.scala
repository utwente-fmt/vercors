package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceBoolResource, CoerceBoundIntFrac, CoerceBoundIntZFrac, CoerceCPrimitiveToCol, CoerceColToCPrimitive, CoerceFloatRat, CoerceFracZFrac, CoerceIdentity, CoerceIntRat, CoerceJavaSupports, CoerceJoinUnion, CoerceMapBag, CoerceMapEither, CoerceMapMap, CoerceMapMatrix, CoerceMapOption, CoerceMapSeq, CoerceMapSet, CoerceMapTuple, CoerceMapType, CoerceNothingSomething, CoerceNullArray, CoerceNullClass, CoerceNullJavaClass, CoerceNullPointer, CoerceNullRef, CoerceRatZFrac, CoerceSelectUnion, CoerceSomethingAny, CoerceSupports, CoerceUnboundInt, CoerceWidenBound, CoerceZFracFrac, CoerceZFracRat, Coercion, CoercionSequence, Type}

trait CoercionImpl[G] { this: Coercion[G] =>
  def target: Type[G]

  def isPromoting: Boolean = this match {
    case CoerceIdentity(_) => true
    case CoercionSequence(coercions) => coercions.forall(_.isPromoting)
    case CoerceNothingSomething(_) => true
    case CoerceSomethingAny(_) => true
    case CoerceJoinUnion(inner, _, _) => inner.forall(_.isPromoting)
    case CoerceSelectUnion(inner, _, _, _) => inner.isPromoting
    case CoerceBoolResource() => true
    case CoerceNullRef() => true
    case CoerceNullArray(_) => true
    case CoerceNullClass(_) => true
    case CoerceNullJavaClass(_) => true
    case CoerceNullPointer(_) => true
    case CoerceFracZFrac() => true
    case CoerceZFracRat() => true
    case CoerceFloatRat() => true
    case CoerceIntRat() => true
    case CoerceWidenBound(_, _) => true
    case CoerceUnboundInt(_) => true
    case CoerceBoundIntFrac() => true
    case CoerceBoundIntZFrac(_) => true
    case CoerceSupports(_, _) => true
    case CoerceJavaSupports(_, _) => true
    case CoerceCPrimitiveToCol(_, _) => true
    case CoerceColToCPrimitive(_, _) => true
    case CoerceMapOption(inner, _, _) => inner.isPromoting
    case CoerceMapTuple(inner, _, _) => inner.forall(_.isPromoting)
    case CoerceMapEither(inner, _, _) => inner._1.isPromoting && inner._2.isPromoting
    case CoerceMapSeq(inner, _, _) => inner.isPromoting
    case CoerceMapSet(inner, _, _) => inner.isPromoting
    case CoerceMapBag(inner, _, _) => inner.isPromoting
    case CoerceMapMatrix(inner, _, _) => inner.isPromoting
    case CoerceMapMap(inner, _, _) => inner.isPromoting
    case CoerceMapType(inner, _, _) => inner.isPromoting
    case CoerceRatZFrac() => false
    case CoerceZFracFrac() => false
  }
}