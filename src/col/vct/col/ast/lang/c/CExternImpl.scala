package vct.col.ast.lang.c

import vct.col.ast.CExtern
import vct.col.ast.ops.CExternOps
import vct.col.print.{Ctx, Doc, Text}

trait CExternImpl[G] extends CExternOps[G] {
  this: CExtern[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("extern")
}
