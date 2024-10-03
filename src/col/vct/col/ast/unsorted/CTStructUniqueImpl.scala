package vct.col.ast.unsorted

import vct.col.ast.CTStructUnique
import vct.col.ast.ops.CTStructUniqueOps
import vct.col.print._

trait CTStructUniqueImpl[G] extends CTStructUniqueOps[G] { this: CTStructUnique[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
