package vct.col.ast.lang

import vct.col.ast.CLong
import vct.col.print.{Ctx, Doc, Text}

trait CLongImpl[G] { this: CLong[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("long")
}