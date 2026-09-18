package vct.col.ast.expr.op.bit

import vct.col.ast.ops.AmbiguousBitShrOps
import vct.col.ast.{AmbiguousBitShr, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait AmbiguousBitShrImpl[G] extends AmbiguousBitShrOps[G] {
  this: AmbiguousBitShr[G] =>
  override def t: Type[G] = getNumericType

  override def precedence: Int = Precedence.SHIFT
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">>", right)
}
