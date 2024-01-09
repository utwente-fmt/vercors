package vct.col.ast.expr.op.cmp

import vct.col.ast.SubSet
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.SubSetOps

trait SubSetImpl[G] extends SubSetOps[G] { this: SubSet[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<", right)
}