package vct.col.ast.family.coercion

import vct.col.ast._
import vct.col.ast.ops.CoercionFamilyOps

trait CoercionImpl[G] extends CoercionFamilyOps[G] {
  this: Coercion[G] =>
  def target: Type[G]

  def isCPromoting: Boolean =
    this match {
      case CoerceDecreasePrecision(_, _) => false
      case CoerceCFloatCInt(_) => false
      case CoercionSequence(coercions) => coercions.forall(_.isCPromoting)
      case CoerceJoinUnion(inner, _, _) => inner.forall(_.isPromoting)
      case CoerceSelectUnion(inner, _, _, _) => inner.isPromoting
      case CoerceMapOption(inner, _, _) => inner.isCPromoting
      case CoerceMapTuple(inner, _, _) => inner.forall(_.isCPromoting)
      case CoerceMapEither(inner, _, _) =>
        inner._1.isCPromoting && inner._2.isCPromoting
      case CoerceMapSeq(inner, _, _) => inner.isCPromoting
      case CoerceMapSet(inner, _, _) => inner.isCPromoting
      case CoerceMapVector(inner, _, _, _) => inner.isCPromoting
      case CoerceMapBag(inner, _, _) => inner.isCPromoting
      case CoerceMapMatrix(inner, _, _) => inner.isCPromoting
      case CoerceMapMap(inner, _, _) => inner.isCPromoting
      case CoerceMapType(inner, _, _) => inner.isCPromoting
      case _ => true
    }

  def isPromoting: Boolean =
    this match {
      case CoerceIdentity(_) => true
      case CoercionSequence(coercions) => coercions.forall(_.isPromoting)
      case CoerceNothingSomething(_) => true
      case CoerceSomethingAny(_) => true
      case CoerceSomethingAnyValue(_) => true
      case CoerceJoinUnion(inner, _, _) => inner.forall(_.isPromoting)
      case CoerceSelectUnion(inner, _, _, _) => inner.isPromoting
      case CoerceBoolResource() => true
      case CoerceNullRef() => true
      case CoerceNullArray(_) => true
      case CoerceNullClass(_, _) => true
      case CoerceNullJavaClass(_) => true
      case CoerceNullAnyClass() => true
      case CoerceNullPointer(_) => true
      case CoerceNullEnum(_) => true
      case CoerceFracZFrac() => true
      case CoerceZFracRat() => true
      case CoerceFloatRat(_) => true
      case CoerceIncreasePrecision(_, _) => true
      case CoerceIntRat() => true
      case CoerceWidenBound(_, _) => true
      case CoerceUnboundInt(_, _) => true
      case CoerceBoundIntFrac() => true
      case CoerceBoundIntZFrac(_) => true
      case CoerceSupports(_, _) => true
      case CoerceJavaSupports(_, _) => true
      case CoerceClassAnyClass(_, _) => true
      case CoerceJavaClassAnyClass(_) => true
      case CoerceCPrimitiveToCol(_, _) => true
      case CoerceColToCPrimitive(_, _) => true
      case CoerceCPPPrimitiveToCol(_, _) => true
      case CoerceColToCPPPrimitive(_, _) => true
      case CoerceCPPArrayPointer(_) => true
      case CoerceCArrayPointer(_) => true
      case CoerceCVectorVector(_, _) => true
      case CoerceResourceResourceVal() => true
      case CoerceResourceValResource() => true
      case CoerceFromConst(_) => true
      case CoerceToConst(_) => true
      case CoerceFromUnique(_, _) => true
      case CoerceToUnique(_, _) => true
      case CoerceBetweenUnique(_, _, inner) => inner.isPromoting
      case CoerceMapOption(inner, _, _) => inner.isPromoting
      case CoerceMapTuple(inner, _, _) => inner.forall(_.isPromoting)
      case CoerceMapEither(inner, _, _) =>
        inner._1.isPromoting && inner._2.isPromoting
      case CoerceMapSeq(inner, _, _) => inner.isPromoting
      case CoerceMapSet(inner, _, _) => inner.isPromoting
      case CoerceMapVector(inner, _, _, _) => inner.isPromoting
      case CoerceMapBag(inner, _, _) => inner.isPromoting
      case CoerceMapMatrix(inner, _, _) => inner.isPromoting
      case CoerceMapMap(inner, _, _) => inner.isPromoting
      case CoerceMapType(inner, _, _) => inner.isPromoting
      case CoerceRatZFrac() => false
      case CoerceZFracFrac() => false
      case CoerceBoundIntFloat(_, _) => false

      case CoerceCIntCFloat(_) => true
      case CoerceCIntInt() => true
      case CoerceCFloatFloat(_, _) => true
      case CoerceDecreasePrecision(_, _) => false
      case CoerceCFloatCInt(_) => false
    }
}
