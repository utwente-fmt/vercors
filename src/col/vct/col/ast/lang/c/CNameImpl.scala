package vct.col.ast.lang.c

import vct.col.ast.CName
import vct.col.ast.ops.CNameOps
import vct.col.print.{Ctx, Doc, Text}

trait CNameImpl[G] extends CNameOps[G] {
  this: CName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
