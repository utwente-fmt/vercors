package vct.col.ast.lang.java

import vct.col.ast.JavaProtected
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaProtectedOps

trait JavaProtectedImpl[G] extends JavaProtectedOps[G] { this: JavaProtected[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("protected")
}