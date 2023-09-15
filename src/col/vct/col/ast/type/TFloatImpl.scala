package vct.col.ast.`type`

import vct.col.ast.{CPrimitiveType, CSpecificationType, TFloat, Type}
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.print.{Ctx, Doc, Empty, Group, Text}
import vct.col.typerules.CoercionUtils

// https://en.wikipedia.org/wiki/Single-precision_floating-point_format#IEEE_754_standard:_binary32
// https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
object TFloats {
  // Integer part bit that is implicit, is included here.
  def ieee754_32bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(8, 24)
  def ieee754_64bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(11, 53)
  def max[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(999, 999)

  // Only works if one if them is guaranteed to be a TFloat
  def coerceToMax[G](l: Type[G], r: Type[G]): Type[G] = (l, r) match {
    case (l @ TFloat(le, lm), r @ TFloat(re, rm)) if re == le && rm == lm => l
    case (l @ TFloat(le, lm), r @ TFloat(re, rm)) if le < re && lm < rm => l
    case (l @ TFloat(le, lm), r @ TFloat(re, rm)) if re < le && rm < lm => r
    case (l @ TFloat(le, lm), r @ TFloat(re, rm)) => ??? // Assuming there is only one TFloat for each exponent, mantissa
    case (CPrimitiveType(Seq(CSpecificationType(inner1))), CPrimitiveType(Seq(CSpecificationType(inner2)))) =>
      val res = coerceToMax(inner1, inner2)
      CPrimitiveType(Seq(CSpecificationType(res)(res.o)))
    case (CPrimitiveType(Seq(CSpecificationType(inner1))), _: TFloat[_]) =>
      val res = coerceToMax(inner1, r)
      CPrimitiveType(Seq(CSpecificationType(res)(res.o)))
    case (_: TFloat[_], CPrimitiveType(Seq(CSpecificationType(inner2)))) =>
      val res = coerceToMax(l, inner2)
      CPrimitiveType(Seq(CSpecificationType(res)(res.o)))
    // If one if them needs to be coerced, we coerce and try again
    case (l @ TFloat(le, lm), r) => coerceToMax(l, CoercionUtils.getCoercion(r, l).get.target)
    case (l, r @ TFloat(re, rm)) => coerceToMax(r, CoercionUtils.getCoercion(l, r).get.target)
    case _ => ???
  }

  def isFloatOp[G](l: Type[G], r: Type[G]): Boolean = (l, r) match {
    case (CPrimitiveType(Seq(CSpecificationType(inner1))), CPrimitiveType(Seq(CSpecificationType(inner2)))) =>
      isFloatOp(inner1, inner2)
    case (CPrimitiveType(Seq(CSpecificationType(inner1))), _) =>
      isFloatOp(inner1, r)
    case (_, CPrimitiveType(Seq(CSpecificationType(inner2)))) =>
      isFloatOp(l, inner2)
    case (t1 @ TFloat(_, _), t2 @ TFloat(_, _)) =>
      CoercionUtils.getCoercion(t1, t2).isDefined || CoercionUtils.getCoercion(t2, t1).isDefined
    case (t, f @ TFloat(_, _)) => CoercionUtils.getCoercion(t, f).isDefined
    case (f @ TFloat(_, _), t) => CoercionUtils.getCoercion(t, f).isDefined
    case _ => false
  }

}

trait TFloatImpl[G] { this: TFloat[G] =>
  assert(this.exponent > 0)
  assert(this.mantissa > 0)

  def layoutFloat(implicit ctx: Ctx): Doc = Text(ctx.syntax match {
    case Ctx.PVL => "float32"
    case Ctx.Java => "float"
    case _ => "float"
  })

  def layoutDouble(implicit ctx: Ctx): Doc = Text(ctx.syntax match {
    case Ctx.PVL => "float64"
    case Ctx.Java => "double"
    case _ => "double"
  })

  override def layout(implicit ctx: Ctx): Doc =
    if(this == TFloats.ieee754_32bit) layoutFloat
    else if(this == TFloats.ieee754_64bit) layoutDouble
    else Text(s"??float_${exponent}_${mantissa}??")
}