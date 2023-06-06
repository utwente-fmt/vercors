package vct.col.ast.lang

import vct.col.ast.JavaStatic
import vct.col.print.{Ctx, Doc, Text}

trait JavaStaticImpl[G] { this: JavaStatic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("static")
}