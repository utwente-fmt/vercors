package vct.col.ast.unsorted

import vct.col.ast.TClassUnique
import vct.col.ast.ops.TClassUniqueOps
import vct.col.print._

trait TClassUniqueImpl[G] extends TClassUniqueOps[G] { this: TClassUnique[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
