package vct.col.ast.unsorted

import vct.col.ast.{AbstractPredicate, ApplyAnyPredicate, FoldTarget}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.FoldTargetFamilyOps
import vct.col.print._

trait FoldTargetImpl[G] extends NodeFamilyImpl[G] with FoldTargetFamilyOps[G] {
  this: FoldTarget[G] =>

  def apply: ApplyAnyPredicate[G]
}
