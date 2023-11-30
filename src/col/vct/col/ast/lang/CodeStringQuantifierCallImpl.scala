package vct.col.ast.lang

import vct.col.ast.CodeStringQuantifierCall
import vct.col.print.{Ctx, Doc, Text}

trait CodeStringQuantifierCallImpl[G] {
  this: CodeStringQuantifierCall[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("content")

}