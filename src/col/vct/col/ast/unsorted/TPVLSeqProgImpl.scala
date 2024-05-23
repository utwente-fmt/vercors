package vct.col.ast.unsorted

import vct.col.ast.TPVLSeqProg
import vct.col.ast.ops.TPVLSeqProgOps
import vct.col.print._

trait TPVLSeqProgImpl[G] extends TPVLSeqProgOps[G] { this: TPVLSeqProg[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
