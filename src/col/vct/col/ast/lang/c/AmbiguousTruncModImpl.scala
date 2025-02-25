package vct.col.ast.lang.c

import vct.col.ast.AmbiguousTruncMod
import vct.col.ast.ops.AmbiguousTruncModOps
import vct.col.print._

trait AmbiguousTruncModImpl[G] extends AmbiguousTruncModOps[G] {
  this: AmbiguousTruncMod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}
