package vct.col.ast.`type`

import vct.col.ast.TFloat
import vct.col.ast.ops.TFloatOps
import vct.col.ast.`type`.typeclass.TFloats
import vct.col.typerules.TypeSize

trait TFloatImpl[G] extends TFloatOps[G] {
  this: TFloat[G] =>
  // TODO: Should this be Minimally?
  override def bits: TypeSize =
    TypeSize.Exact(BigInt.int2bigInt(exponent + mantissa))
  override def is_ieee754_32bit: Boolean = this == TFloats.ieee754_32bit

  override def is_ieee754_64bit: Boolean = this == TFloats.ieee754_64bit
}
