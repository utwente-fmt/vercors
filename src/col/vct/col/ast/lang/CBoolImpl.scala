package vct.col.ast.lang

import vct.col.ast.CBool
import vct.col.print.{Ctx, Doc, Text}

trait CBoolImpl[G] { this: CBool[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("_Bool")
}