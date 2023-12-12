package vct.col.ast.`type`

import vct.col.ast.{TFloat}
import vct.col.ast.`type`.typeclass.TFloats

trait TFloatImpl[G] { this: TFloat[G] =>
  override def is_ieee754_32bit: Boolean = this == TFloats.ieee754_32bit

  override def is_ieee754_64bit: Boolean = this == TFloats.ieee754_64bit
}