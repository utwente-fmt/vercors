package vct.col.ast.unsorted

import vct.col.ast.CCast
import vct.col.ast.ops.CCastOps
import vct.col.print._

trait CCastImpl[G] extends CCastOps[G] { this: CCast[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
