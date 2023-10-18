package vct.col.ast.lang

import vct.col.ast.CPPInline
import vct.col.print.{Ctx, Doc, Text}

trait CPPInlineImpl[G] {
  this: CPPInline[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("inline")
}
