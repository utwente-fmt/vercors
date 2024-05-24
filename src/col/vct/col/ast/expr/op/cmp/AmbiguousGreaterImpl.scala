package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousGreater
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.AmbiguousGreaterOps

trait AmbiguousGreaterImpl[G] extends AmbiguousGreaterOps[G] { this: AmbiguousGreater[G] =>
  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, ">", right)
}