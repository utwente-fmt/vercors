package vct.col.ast.lang

import vct.col.ast.JavaPrivate
import vct.col.print.{Ctx, Doc, Text}

trait JavaPrivateImpl[G] {
  this: JavaPrivate[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("private")
}
