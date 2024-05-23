package vct.col.ast.unsorted

import vct.col.ast.UnresolvedChorLoop
import vct.col.print._
import vct.col.ast.ops.UnresolvedChorLoopOps

trait UnresolvedChorLoopImpl[G] extends UnresolvedChorLoopOps[G] { this: UnresolvedChorLoop[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
