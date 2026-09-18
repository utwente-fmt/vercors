package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMTFloatOps
import vct.col.ast._
import vct.col.print._
import vct.col.typerules.TypeSize

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
  override lazy val mantissa: Int =
    floatType match {
      case F16() => 10
      case BF16() => 7
      case F32() => 23
      case F64() => 52
      case F80() => 64
      case F128() => 112
      // TODO: This is variable, see https://www.ibm.com/docs/en/aix/7.3?topic=sepl-128-bit-long-double-floating-point-data-type
      case PPCF128() => ???
    }

  override def layout(implicit ctx: Ctx): Doc = floatType.show
  override def bits: TypeSize = TypeSize.Exact(1 + exponent + mantissa)
}
