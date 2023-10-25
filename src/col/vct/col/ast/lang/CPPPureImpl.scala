package vct.col.ast.lang

import vct.col.ast.CPPPure
import vct.col.print.{Ctx, Doc, Text}

trait CPPPureImpl[G] { this: CPPPure[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("pure"))
}