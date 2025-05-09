package vct.col.ast.lang.c

import vct.col.ast.COpaque
import vct.col.ast.ops.COpaqueOps
import vct.col.print._

trait COpaqueImpl[G] extends COpaqueOps[G] {
  this: COpaque[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("opaque")
}
