package vct.col.ast.expr.op.bit

import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.ops.BitAndOps
import vct.col.ast.{BitAnd, TCInt, Type}
import vct.col.print._
import vct.col.typerules.TypeSize

trait BitAndImpl[G] extends BitAndOps[G] {
  this: BitAnd[G] =>
  override def t: Type[G] =
    getNumericType match {
      case t: TCInt[G] if BinOperatorTypes.getBits(t) != 0 && bits != 0 =>
        t.storedBits = TypeSize.Exact(bits)
        t
      case t => t
    }

  override def precedence: Int = Precedence.BIT_AND

  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "&", right)
}
