package vct.col.ast.lang.c

import vct.col.ast.CTypedef
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CTypedefOps

trait CTypedefImpl[G] extends CTypedefOps[G] {
  this: CTypedef[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("typedef")
}
