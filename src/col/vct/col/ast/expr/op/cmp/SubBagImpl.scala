package vct.col.ast.expr.op.cmp

import vct.col.ast.SubBag
import vct.col.print.{Ctx, Doc, Precedence}

trait SubBagImpl[G] { this: SubBag[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<", right)
}
