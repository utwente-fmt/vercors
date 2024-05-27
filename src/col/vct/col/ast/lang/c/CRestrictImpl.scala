package vct.col.ast.lang.c

import vct.col.ast.CRestrict
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CRestrictOps

trait CRestrictImpl[G] extends CRestrictOps[G] {
  this: CRestrict[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("restrict")
}
