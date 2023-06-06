package vct.col.ast.lang

import vct.col.ast.JavaWildcard
import vct.col.print.{Ctx, Doc, Text}

trait JavaWildcardImpl[G] { this: JavaWildcard[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("?")
}
