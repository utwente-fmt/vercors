package vct.col.rewrite

import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, RewriterBuilder}
import vct.col.typerules.CoercingRewriter

case object CFloatIntCoercion extends RewriterBuilder {
  override def key: String = "CFloatIntCoercion"
  override def desc: String =
    "Places cast from ints and floats from the C backend."
}

case class CFloatIntCoercion[Pre <: Generation]()
    extends CoercingRewriter[Pre] {
  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceCFloatCInt(_) => CastFloat(e, TInt())
      case CoerceCIntCFloat(target) => CastFloat(e, dispatch(target))
      case CoerceDecreasePrecision(_, target) => CastFloat(e, dispatch(target))
      case c if ignoreMappedCoercion(c) => e
      case other => super.applyCoercion(e, other)
    }

  def isIgnoredCoercion(c: Coercion[_]): Boolean =
    c match {
      case CoerceCFloatFloat(_, _) => true
      case CoerceCIntInt(_) => true
      case CoerceIncreasePrecision(_, _) => true
      case CoerceIncreasePrecision(_, _) => true
      case _ => false
    }

  def ignoreMappedCoercion(c: Coercion[_]): Boolean =
    c match {
      case CoerceMapSeq(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapSet(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapBag(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapMap(inner, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapTuple(inner, _, _) if inner.forall(isIgnoredCoercion) =>
        true
      case CoerceMapVector(inner, _, _, _) if isIgnoredCoercion(inner) => true
      case CoerceMapOption(inner, _, _) if isIgnoredCoercion(inner) => true
      case _ => false
    }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TCInt() => TInt()
      // This is wrong, but since we translate to rationals anyways, this does not matter.
      // Getting everything to type check otherwise is a pain, since in "coerce" we always coerce
      // to an arbitrary big float.
      case TCFloat(_, _) => TFloats.ieee754_32bit
      case TFloat(_, _) => TFloats.ieee754_32bit
      case other => other.rewriteDefault()
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] =
    e match {
      // TODO: Do truncation/sign extension
      case Cast(e, TypeValue(TCInt())) if e.t.isInstanceOf[TCInt[Pre]] =>
        dispatch(e)
      case CIntegerValue(v, _) => IntegerValue(v)(e.o)
      case other => other.rewriteDefault()
    }
}
