package vct.col.ast.lang

import vct.col.ast.{CodeString, CodeStringQuantifier}
import vct.col.print.{Ctx, Doc, Text}

trait CodeStringQuantifierImpl[G] {
  this: CodeStringQuantifier[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("content")

}