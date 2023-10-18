package vct.col.ast.lang

import vct.col.ast.CPure
import vct.col.print.{Ctx, Doc, Text}

trait CPureImpl[G] {
  this: CPure[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("pure"))
}
