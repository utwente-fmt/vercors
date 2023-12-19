package vct.col.ast.expr.op.cmp

import vct.col.ast.SubBag
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.SubBagOps

trait SubBagImpl[G] extends SubBagOps[G] { this: SubBag[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<", right)
}
