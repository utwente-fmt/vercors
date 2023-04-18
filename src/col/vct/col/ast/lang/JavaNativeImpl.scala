package vct.col.ast.lang

import vct.col.ast.JavaNative
import vct.col.print.{Ctx, Doc, Text}

trait JavaNativeImpl[G] { this: JavaNative[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("native")
}