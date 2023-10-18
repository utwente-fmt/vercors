package vct.col.ast.lang

import vct.col.ast.CPPName
import vct.col.print.{Ctx, Doc, Text}

trait CPPNameImpl[G] {
  this: CPPName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
