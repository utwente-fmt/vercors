package vct.col.ast.lang.cpp

import vct.col.ast.CPPInline
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPInlineOps

trait CPPInlineImpl[G] extends CPPInlineOps[G] { this: CPPInline[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("inline")
}