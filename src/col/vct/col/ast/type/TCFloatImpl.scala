package vct.col.ast.`type`

import vct.col.ast.{TCFloat, TFloat}
import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast.ops.TCFloatOps

object TCFloats {
  def fromTCFloat[G](t: TCFloat[G]): TFloat[G] =
    TFloat(t.exponent, t.mantissa)(t.o)
}

trait TCFloatImpl[G] extends TCFloatOps[G] {
  this: TCFloat[G] =>

  def is_ieee754_32bit: Boolean = this == TFloats.C_ieee754_32bit[G]

  def is_ieee754_64bit: Boolean = this == TFloats.C_ieee754_64bit[G]
}
