package vct.col.ast.lang

import vct.col.ast.CName
import vct.col.print.{Ctx, Doc, Text}

trait CNameImpl[G] { this: CName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(name)
}