package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMFloatValueOps
import vct.col.ast.{
  BF16,
  F128,
  F16,
  F32,
  F64,
  F80,
  LLVMFloatType,
  LLVMFloatValue,
  LLVMTFloat,
  PPCF128,
  Type,
}
import vct.col.origin.Origin
import vct.col.print._
import vct.result.VerificationError.UserError

trait LLVMFloatValueImpl[G] extends LLVMFloatValueOps[G] {
  this: LLVMFloatValue[G] =>
  override def t: Type[G] = LLVMTFloat(floatType)
  override def layout(implicit ctx: Ctx): Doc = Text(bigDecimalValue.toString)

  // We could use a library like https://github.com/FirebirdSQL/decimal-java/ which also supports F128
  lazy val bigDecimalValue: BigDecimal =
    floatType match {
      case F16() => throw UnsupportedFloatType(floatType)
      case BF16() => throw UnsupportedFloatType(floatType)
      case F32() =>
        BigDecimal.valueOf(java.lang.Float.intBitsToFloat(value.intValue))
      case F64() =>
        BigDecimal.valueOf(java.lang.Double.longBitsToDouble(value.longValue))
      case F80() => throw UnsupportedFloatType(floatType)
      case F128() => throw UnsupportedFloatType(floatType)
      case PPCF128() => throw UnsupportedFloatType(floatType)
    }

  // Un-override the toString method overridden in Constant
  override def toString: String = toStringWithContext(Ctx().namesIn(this))
}

private final case class UnsupportedFloatType(floatType: LLVMFloatType[_])(
    implicit val o: Origin
) extends UserError {
  override def code: String = "llvmUnsupportedFloatType"

  override def text: String =
    o.messageInContext(
      f"VerCors currently does not implement support for the float type: $floatType"
    )
}
