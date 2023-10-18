package vct.col.ast.`type`

import vct.col.ast.TAny
import vct.col.print.{Ctx, Doc, Text}

trait TAnyImpl[G] {
  this: TAny[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("any")
}
