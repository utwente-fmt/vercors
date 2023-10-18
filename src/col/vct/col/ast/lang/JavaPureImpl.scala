package vct.col.ast.lang

import vct.col.ast.JavaPure
import vct.col.print.{Ctx, Doc, Text}

trait JavaPureImpl[G] {
  this: JavaPure[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("pure"))
}
