package vct.col.ast.lang

import vct.col.ast.CPPPointer
import vct.col.print.{Ctx, Doc, Text}

trait CPPPointerImpl[G] {
  this: CPPPointer[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("*")
}
