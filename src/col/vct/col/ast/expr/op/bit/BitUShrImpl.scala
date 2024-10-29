package vct.col.ast.expr.op.bit

import vct.col.ast.{BitUShr, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.BitUShrOps

trait BitUShrImpl[G] extends BitUShrOps[G] {
  this: BitUShr[G] =>
  override def t: Type[G] = getIntType

  override def precedence: Int = Precedence.SHIFT
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">>>", right)
}
