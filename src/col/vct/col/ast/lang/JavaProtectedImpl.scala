package vct.col.ast.lang

import vct.col.ast.JavaProtected
import vct.col.print.{Ctx, Doc, Text}

trait JavaProtectedImpl[G] {
  this: JavaProtected[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("protected")
}
