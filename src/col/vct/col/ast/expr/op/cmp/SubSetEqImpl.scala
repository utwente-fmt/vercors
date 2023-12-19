package vct.col.ast.expr.op.cmp

import vct.col.ast.SubSetEq
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.SubSetEqOps

trait SubSetEqImpl[G] extends SubSetEqOps[G] { this: SubSetEq[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<=", right)
}