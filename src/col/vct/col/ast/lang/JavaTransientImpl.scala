package vct.col.ast.lang

import vct.col.ast.JavaTransient
import vct.col.print.{Ctx, Doc, Text}

trait JavaTransientImpl[G] { this: JavaTransient[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("transient")
}