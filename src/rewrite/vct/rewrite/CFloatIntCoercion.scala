package vct.col.rewrite

import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast.{
  BinExpr,
  CIntegerValue,
  CastFloat,
  CoerceCFloatCInt,
  CoerceCIntCFloat,
  CoerceDecreasePrecision,
  Coercion,
  Expr,
  IntegerValue,
  Loop,
  ProcedureInvocation,
  SimplifiedProcedureInvocation,
  SimplifiedLoop,
  Statement,
  TCFloat,
  TCInt,
  TFloat,
  TInt,
  Type,
}
import vct.col.origin.Origin
import vct.col.ref.Ref
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
      case other => super.applyCoercion(e, other)
    }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TCInt() => TInt()
      // This is wrong, but since we translate to rationals anyways, this does not matter.
      // Getting everything to type check otherwise is a pain, since in "coerce" we always coerce
      // to an arbitrary big float.
      case TCFloat(e, m) => TFloats.ieee754_32bit
      case TFloat(e, m) => TFloats.ieee754_32bit
      case other => other.rewriteDefault()
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] =
    e match {
      case CIntegerValue(v) => IntegerValue(v)(e.o)
      // This has nothing to do with Floats and Ints, but gets rid of C-specific stuff,
      // so I'll add it here for now
      case s @ SimplifiedProcedureInvocation(Ref(proc), args) =>
        ProcedureInvocation[Post](
          succ(proc),
          args.map(dispatch),
          Nil,
          Nil,
          Nil,
          Nil,
        )(s.blame)(s.o)
      case other => other.rewriteDefault()
    }

  // This has nothing to do with Floats and Ints, but gets rid of C-specific stuff,
  // so I'll add it here for now
  override def postCoerce(s: Statement[Pre]): Statement[Post] = {
    s match {
      case SimplifiedLoop(init, cond, update, contract, body) =>
        Loop[Post](
          dispatch(init),
          dispatch(cond),
          dispatch(update),
          dispatch(contract),
          dispatch(body),
        )(s.o)
      case other => other.rewriteDefault()
    }
  }
}
