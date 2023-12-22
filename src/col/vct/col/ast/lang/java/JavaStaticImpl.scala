package vct.col.ast.lang.java

import vct.col.ast.JavaStatic
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaStaticOps

trait JavaStaticImpl[G] extends JavaStaticOps[G] { this: JavaStatic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("static")
}