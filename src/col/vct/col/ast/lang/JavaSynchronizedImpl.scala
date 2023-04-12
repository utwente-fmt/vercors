package vct.col.ast.lang

import vct.col.ast.JavaSynchronized
import vct.col.print.{Ctx, Doc, Text}

trait JavaSynchronizedImpl[G] { this: JavaSynchronized[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("synchronized")
}