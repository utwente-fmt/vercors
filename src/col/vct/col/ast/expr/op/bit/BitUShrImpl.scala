package vct.col.ast.expr.op.bit

import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.ops.BitUShrOps
import vct.col.ast.{BitUShr, TCInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.typerules.TypeSize

trait BitUShrImpl[G] extends BitUShrOps[G] {
  this: BitUShr[G] =>
  override def t: Type[G] =
    getNumericType match {
      case t: TCInt[G] if BinOperatorTypes.getBits(t) != 0 && bits != 0 =>
        t.storedBits = TypeSize.Exact(bits)
        t
      case t => t
    }

  override def precedence: Int = Precedence.SHIFT
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">>>", right)
}
