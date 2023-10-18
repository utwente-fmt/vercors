package vct.col.ast.lang

import vct.col.ast.CConst
import vct.col.print.{Ctx, Doc, Text}

trait CConstImpl[G] {
  this: CConst[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("const")
}
