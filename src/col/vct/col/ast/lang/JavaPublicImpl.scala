package vct.col.ast.lang

import vct.col.ast.JavaPublic
import vct.col.print.{Ctx, Doc, Text}

trait JavaPublicImpl[G] {
  this: JavaPublic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("public")
}
