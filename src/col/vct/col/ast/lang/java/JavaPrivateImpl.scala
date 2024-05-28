package vct.col.ast.lang.java

import vct.col.ast.JavaPrivate
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaPrivateOps

trait JavaPrivateImpl[G] extends JavaPrivateOps[G] {
  this: JavaPrivate[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("private")
}
