package vct.col.ast.lang

import vct.col.ast.CPPLong
import vct.col.print.{Ctx, Doc, Text}

trait CPPLongImpl[G] {
  this: CPPLong[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("long")
}
