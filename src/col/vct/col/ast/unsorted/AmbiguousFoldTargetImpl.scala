package vct.col.ast.unsorted

import vct.col.ast.AmbiguousFoldTarget
import vct.col.ast.ops.AmbiguousFoldTargetOps
import vct.col.print._

trait AmbiguousFoldTargetImpl[G] extends AmbiguousFoldTargetOps[G] { this: AmbiguousFoldTarget[G] =>
   override def layout(implicit ctx: Ctx): Doc = target.show
}
