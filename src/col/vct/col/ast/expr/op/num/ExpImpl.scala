package vct.col.ast.expr.op.num

import vct.col.ast.Exp
import vct.col.print.{Ctx, Doc, Precedence}

trait ExpImpl[G] {
  this: Exp[G] =>
  override def precedence: Int = Precedence.PVL_POW
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "^^", right)
}
