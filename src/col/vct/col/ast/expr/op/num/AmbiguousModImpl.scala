package vct.col.ast.expr.op.num

import vct.col.ast.AmbiguousMod
import vct.col.ast.ops.AmbiguousModOps
import vct.col.print._

trait AmbiguousModImpl[G] extends AmbiguousModOps[G] { this: AmbiguousMod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}
