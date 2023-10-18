package vct.col.ast.lang

import vct.col.ast.CInt
import vct.col.print.{Ctx, Doc, Text}

trait CIntImpl[G] {
  this: CInt[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}
