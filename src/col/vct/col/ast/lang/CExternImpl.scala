package vct.col.ast.lang

import vct.col.ast.CExtern
import vct.col.print.{Ctx, Doc, Text}

trait CExternImpl[G] {
  this: CExtern[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("extern")
}
