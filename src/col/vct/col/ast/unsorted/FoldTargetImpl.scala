package vct.col.ast.unsorted

import vct.col.ast.FoldTarget
import vct.col.ast.ops.FoldTargetFamilyOps
import vct.col.print._

trait FoldTargetImpl[G] extends FoldTargetFamilyOps[G] {
  this: FoldTarget[G] =>

}
