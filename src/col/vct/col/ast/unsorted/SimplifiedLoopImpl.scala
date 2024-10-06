package vct.col.ast.unsorted

import vct.col.ast.SimplifiedLoop
import vct.col.ast.ops.SimplifiedLoopOps
import vct.col.print._

trait SimplifiedLoopImpl[G] extends SimplifiedLoopOps[G] {
  this: SimplifiedLoop[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
