package vct.col.ast.lang

import vct.col.ast.CVolatile
import vct.col.print.{Ctx, Doc, Text}

trait CVolatileImpl[G] {
  this: CVolatile[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("volatile")
}
