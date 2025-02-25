package vct.col.ast.lang.c

import vct.col.ast.CVolatile
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CVolatileOps

trait CVolatileImpl[G] extends CVolatileOps[G] {
  this: CVolatile[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("volatile")
}
