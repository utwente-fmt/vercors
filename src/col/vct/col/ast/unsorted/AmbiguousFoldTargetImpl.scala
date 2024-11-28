package vct.col.ast.unsorted

import vct.col.ast.{AmbiguousFoldTarget, ApplyAnyPredicate}
import vct.col.ast.ops.AmbiguousFoldTargetOps
import vct.col.print._

trait AmbiguousFoldTargetImpl[G] extends AmbiguousFoldTargetOps[G] {
  this: AmbiguousFoldTarget[G] =>
  override def layout(implicit ctx: Ctx): Doc = target.show

  // Not supported, until someone implements it
  def apply: ApplyAnyPredicate[G] = ???
}
