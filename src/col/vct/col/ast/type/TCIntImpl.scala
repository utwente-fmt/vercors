package vct.col.ast.`type`

import vct.col.ast.TCInt
import vct.col.print.{Ctx, Doc, Text}

trait TCIntImpl[G] { this: TCInt[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}