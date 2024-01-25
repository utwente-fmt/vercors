package vct.col.ast.lang.runtime

import vct.col.ast.CodeString
import vct.col.print.{Ctx, Doc, Text}

trait CodeStringImpl[G] {
  this: CodeString[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(content)

}