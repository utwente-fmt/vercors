package vct.col.ast.lang

import vct.col.ast.CChar
import vct.col.print.{Ctx, Doc, Text}

trait CCharImpl[G] {
  this: CChar[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("char")
}
