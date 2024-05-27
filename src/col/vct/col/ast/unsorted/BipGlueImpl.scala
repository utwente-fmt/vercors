package vct.col.ast.unsorted

import vct.col.ast.BipGlue
import vct.col.ast.ops.BipGlueOps
import vct.col.print._

trait BipGlueImpl[G] extends BipGlueOps[G] {
  this: BipGlue[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
