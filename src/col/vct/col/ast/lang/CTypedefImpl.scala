package vct.col.ast.lang

import vct.col.ast.CTypedef
import vct.col.print.{Ctx, Doc, Text}

trait CTypedefImpl[G] {
  this: CTypedef[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("typedef")
}
