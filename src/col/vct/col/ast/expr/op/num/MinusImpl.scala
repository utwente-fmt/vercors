package vct.col.ast.expr.op.num

import vct.col.ast.Minus
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.MinusOps

trait MinusImpl[G] extends MinusOps[G] { this: Minus[G] =>
  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "-", right)
}