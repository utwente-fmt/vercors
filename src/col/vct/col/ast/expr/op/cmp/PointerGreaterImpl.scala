package vct.col.ast.expr.op.cmp

import vct.col.ast.PointerGreater
import vct.col.ast.ops.PointerGreaterOps
import vct.col.print.{Ctx, Doc, Precedence}

trait PointerGreaterImpl[G] extends PointerGreaterOps[G] {
  this: PointerGreater[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">", right)
}
