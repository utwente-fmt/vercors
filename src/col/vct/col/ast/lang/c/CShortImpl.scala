package vct.col.ast.lang.c

import vct.col.ast.CShort
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CShortOps

trait CShortImpl[G] extends CShortOps[G] {
  this: CShort[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("short")
}
