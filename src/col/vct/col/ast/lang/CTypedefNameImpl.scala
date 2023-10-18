package vct.col.ast.lang

import vct.col.ast.CTypedefName
import vct.col.print.{Ctx, Doc, Text}

trait CTypedefNameImpl[G] {
  this: CTypedefName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
