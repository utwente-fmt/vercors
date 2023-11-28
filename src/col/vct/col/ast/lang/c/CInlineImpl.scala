package vct.col.ast.lang.c

import vct.col.ast.CInline
import vct.col.ast.ops.CInlineOps
import vct.col.print.{Ctx, Doc, Text}

trait CInlineImpl[G] extends CInlineOps[G] { this: CInline[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("inline")
}