package vct.col.ast.lang.java

import vct.col.ast.JavaVolatile
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaVolatileOps

trait JavaVolatileImpl[G] extends JavaVolatileOps[G] {
  this: JavaVolatile[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("volatile")
}
