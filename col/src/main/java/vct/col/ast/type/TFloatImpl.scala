package vct.col.ast.`type`

import vct.col.ast.TFloat
import vct.col.origin.{DiagnosticOrigin, Origin}

// https://en.wikipedia.org/wiki/Single-precision_floating-point_format#IEEE_754_standard:_binary32
// https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
// https://en.wikipedia.org/wiki/Extended_precision#x86_extended_precision_format
object TFloats {
  // Integer part bit that is implicit, is included here.
  def ieee754_32bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(8, 24)
  def ieee754_64bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(11, 53)
  /* TODO: The integer part is exlicit in 80-bit encoding - adjust implementation to this.
        Should probably split TFloat out as a node family, with a set of float types where the integer part is explicit, and
        one where it is implicit. Since we don't have a semantics that we want to check a distinction is not made now.
   */
  def x86extended_80bit[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(11, 62)
  def max[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = vct.col.ast.TFloat(999, 999)
}

trait TFloatImpl[G] { this: TFloat[G] =>
  assert(this.exponent > 0)
  assert(this.mantissa > 0)
}