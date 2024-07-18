package vct.col.ast.declaration.singular

import vct.col.ast.BipGlue
import vct.col.ast.ops.BipGlueOps

trait BipGlueImpl[G] extends BipGlueOps[G] {
  this: BipGlue[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
