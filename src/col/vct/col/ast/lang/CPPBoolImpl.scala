package vct.col.ast.lang

import vct.col.ast.CPPBool
import vct.col.print.{Ctx, Doc, Text}

trait CPPBoolImpl[G] {
  this: CPPBool[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("_Bool")
}
