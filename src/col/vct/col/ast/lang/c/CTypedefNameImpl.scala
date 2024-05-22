package vct.col.ast.lang.c

import vct.col.ast.CTypedefName
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CTypedefNameOps

trait CTypedefNameImpl[G] extends CTypedefNameOps[G] { this: CTypedefName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(name)
}