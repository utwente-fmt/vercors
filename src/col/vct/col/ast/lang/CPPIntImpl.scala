package vct.col.ast.lang

import vct.col.ast.CPPInt
import vct.col.print.{Ctx, Doc, Text}

trait CPPIntImpl[G] {
  this: CPPInt[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}
