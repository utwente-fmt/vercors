package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMTFloatOps
import vct.col.ast._
import vct.col.print._

object LLVMTFloats {
  def fromLLVMTFloat[G](t: LLVMTFloat[G]): TFloat[G] =
    TFloat(t.exponent, t.mantissa)(t.o)
}

trait LLVMTFloatImpl[G] extends LLVMTFloatOps[G] {
  this: LLVMTFloat[G] =>
  override lazy val exponent: Int =
    floatType match {
      case F16() => 5
      case BF16() => 8
      case F32() => 8
      case F64() => 11
      case F80() => 15
      case F128() => 15
      // TODO: See https://www.ibm.com/docs/en/aix/7.3?topic=sepl-128-bit-long-double-floating-point-data-type
      case PPCF128() => ???
    }

  override val is_ieee754_32bit: Boolean = floatType == F32[G]()
  override val is_ieee754_64bit: Boolean = floatType == F64[G]()
  // Integer part bit that is implicit, is included here.
  override lazy val mantissa: Int =
    floatType match {
      case F16() => 11
      case BF16() => 8
      case F32() => 24
      case F64() => 53
      // Special in the sense that the 64th bit is actually stored (if I'm reading wikipedia correctly)
      case F80() => 64
      case F128() => 113
      // TODO: This is variable, see https://www.ibm.com/docs/en/aix/7.3?topic=sepl-128-bit-long-double-floating-point-data-type
      case PPCF128() => ???
    }

  override def layout(implicit ctx: Ctx): Doc = floatType.show
}
