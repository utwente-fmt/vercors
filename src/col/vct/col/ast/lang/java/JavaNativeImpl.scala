package vct.col.ast.lang.java

import vct.col.ast.JavaNative
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaNativeOps

trait JavaNativeImpl[G] extends JavaNativeOps[G] { this: JavaNative[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("native")
}