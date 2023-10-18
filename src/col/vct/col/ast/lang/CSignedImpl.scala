package vct.col.ast.lang

import vct.col.ast.CSigned
import vct.col.print.{Ctx, Doc, Text}

trait CSignedImpl[G] {
  this: CSigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("signed")
}
