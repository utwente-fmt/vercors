package vct.col.ast.lang.c

import vct.col.ast.CConst
import vct.col.ast.ops.CConstOps
import vct.col.print.{Ctx, Doc, Text}

trait CConstImpl[G] extends CConstOps[G] {
  this: CConst[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("const")
}
