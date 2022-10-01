package vct.col.ast.`type`

import vct.col.ast.TFloat
import vct.col.origin.{DiagnosticOrigin, Origin}

// https://en.wikipedia.org/wiki/Single-precision_floating-point_format#IEEE_754_standard:_binary32
// https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
object TFloats {
  // Integer part bit that is implicit, is included here.
  def ieee754_32bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(8, 24)
  def ieee754_64bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(11, 53)
  def max[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(999, 999)

  def mostPrecise[G](l: TFloat[G], r: TFloat[G]) = if (l.exponent < r.exponent && l.mantissa < r.mantissa) {
    r
  } else {
    assert(l.exponent > r.exponent && l.mantissa > r.mantissa)
    l
  }
}

trait TFloatImpl[G] { this: TFloat[G] =>
  assert(this.exponent > 0)
  assert(this.mantissa > 0)
}