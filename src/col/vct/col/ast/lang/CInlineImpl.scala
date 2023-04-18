package vct.col.ast.lang

import vct.col.ast.CInline
import vct.col.print.{Ctx, Doc, Text}

trait CInlineImpl[G] { this: CInline[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("inline")
}