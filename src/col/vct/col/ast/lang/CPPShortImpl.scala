package vct.col.ast.lang

import vct.col.ast.CPPShort
import vct.col.print.{Ctx, Doc, Text}

trait CPPShortImpl[G] {
  this: CPPShort[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("short")
}
