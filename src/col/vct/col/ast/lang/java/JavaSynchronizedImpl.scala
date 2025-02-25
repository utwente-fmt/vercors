package vct.col.ast.lang.java

import vct.col.ast.JavaSynchronized
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaSynchronizedOps

trait JavaSynchronizedImpl[G] extends JavaSynchronizedOps[G] {
  this: JavaSynchronized[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("synchronized")
}
