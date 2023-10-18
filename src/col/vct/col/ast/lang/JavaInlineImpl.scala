package vct.col.ast.lang

import vct.col.ast.JavaInline
import vct.col.print.{Ctx, Doc, Text}

trait JavaInlineImpl[G] {
  this: JavaInline[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("inline")
}
