package vct.col.ast.lang.java

import vct.col.ast.JavaPublic
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaPublicOps

trait JavaPublicImpl[G] extends JavaPublicOps[G] { this: JavaPublic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("public")
}