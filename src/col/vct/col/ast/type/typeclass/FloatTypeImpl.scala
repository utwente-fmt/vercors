package vct.col.ast.`type`.typeclass

import vct.col.ast.`type`.TCFloats.fromTCFloat
import vct.col.ast.{CPrimitiveType, CSpecificationType, FloatType, TCFloat, TFloat, Type}
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.typerules.CoercionUtils
import vct.col.resolve.lang.C

// https://en.wikipedia.org/wiki/Single-precision_floating-point_format#IEEE_754_standard:_binary32
// https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
object TFloats {
  // Integer part bit that is implicit, is included here.
  def ieee754_32bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(8, 24)
  def ieee754_64bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(11, 53)
  def C_ieee754_32bit[G](implicit o: Origin = DiagnosticOrigin): TCFloat[G] = vct.col.ast.TCFloat(8, 24)
  def C_ieee754_64bit[G](implicit o: Origin = DiagnosticOrigin): TCFloat[G] = vct.col.ast.TCFloat(11, 53)

  // Only returns a float, when one of the types is floating itself
  // Returns to biggest float, e.g. (float32, float64) should return float64
  def getFloatMax[G](ll: Type[G], rr: Type[G]): Option[FloatType[G]] = {
    val l = C.stripCPrimitiveType(ll)
    val r = C.stripCPrimitiveType(rr)
    val promote = (l, r) match {
      case (_: TFloat[G], _) => true
      case (_, _: TFloat[G]) => true
      case _ => false
    }
    val promoting: FloatType[G] => FloatType[G] = {
      case t: TCFloat[G] => if(promote) fromTCFloat(t) else t
      case t: TFloat[G] => t
    }

    (l, r) match {
      case (l: FloatType[G], r: FloatType[G]) if r.exponent == l.exponent && r.mantissa == l.mantissa => Some(promoting(l))
      case (l: FloatType[G], r: FloatType[G]) if l.exponent < r.exponent && l.mantissa < r.mantissa => Some(promoting(r))
      case (l: FloatType[G], r: FloatType[G]) if r.exponent < l.exponent && r.mantissa < l.mantissa => Some(promoting(l))
      case (l: FloatType[G], r: FloatType[G]) => ??? // Assuming there is only one Float for each exponent, mantissa
      // If one if them needs to be coerced, we coerce and try again
      case (l: FloatType[G], r) => CoercionUtils.getCoercion(r, l).flatMap(c => getFloatMax(l, c.target))
      case (l, r: FloatType[G]) => CoercionUtils.getCoercion(l, r).flatMap(c => getFloatMax(r, c.target))
      case _ => None
    }
  }
}

trait FloatTypeImpl[G] { this: FloatType[G] =>
  val exponent: Int
  val mantissa: Int
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

  def is_ieee754_32bit: Boolean
  def is_ieee754_64bit: Boolean

  override def layout(implicit ctx: Ctx): Doc =
    if (is_ieee754_32bit) layoutFloat
    else if (is_ieee754_64bit) layoutDouble
    else Text(s"??float_${exponent}_${mantissa}??")
}
