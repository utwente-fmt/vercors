package vct.col.ast.lang

import vct.col.ast.CRestrict
import vct.col.print.{Ctx, Doc, Text}

trait CRestrictImpl[G] {
  this: CRestrict[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("restrict")
}
