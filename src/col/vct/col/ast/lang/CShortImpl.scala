package vct.col.ast.lang

import vct.col.ast.CShort
import vct.col.print.{Ctx, Doc, Text}

trait CShortImpl[G] { this: CShort[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("short")
}