package vct.col.ast.expr.op.bit

import vct.col.ast.{BitShr, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.BitShrOps

trait BitShrImpl[G] extends BitShrOps[G] {
  this: BitShr[G] =>
  override def t: Type[G] = getIntType

  override def precedence: Int = Precedence.SHIFT
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">>", right)
}
