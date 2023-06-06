package vct.col.ast.lang

import vct.col.ast.JavaVolatile
import vct.col.print.{Ctx, Doc, Text}

trait JavaVolatileImpl[G] { this: JavaVolatile[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("volatile")
}