package vct.col.ast.lang.java

import vct.col.ast.JavaTransient
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaTransientOps

trait JavaTransientImpl[G] extends JavaTransientOps[G] { this: JavaTransient[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("transient")
}