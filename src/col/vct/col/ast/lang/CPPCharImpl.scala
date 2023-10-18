package vct.col.ast.lang

import vct.col.ast.CPPChar
import vct.col.print.{Ctx, Doc, Text}

trait CPPCharImpl[G] {
  this: CPPChar[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("char")
}
