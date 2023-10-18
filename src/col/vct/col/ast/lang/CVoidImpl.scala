package vct.col.ast.lang

import vct.col.ast.CVoid
import vct.col.print.{Ctx, Doc, Text}

trait CVoidImpl[G] {
  this: CVoid[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("void")
}
