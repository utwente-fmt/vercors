package vct.col.ast.lang.java

import vct.col.ast.JavaInline
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaInlineOps

trait JavaInlineImpl[G] extends JavaInlineOps[G] { this: JavaInline[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("inline")
}