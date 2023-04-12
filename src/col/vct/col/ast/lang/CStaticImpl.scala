package vct.col.ast.lang

import vct.col.ast.CStatic
import vct.col.print.{Ctx, Doc, Text}

trait CStaticImpl[G] { this: CStatic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("static")
}