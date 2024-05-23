package vct.col.ast.lang.c

import vct.col.ast.CChar
import vct.col.ast.ops.CCharOps
import vct.col.print.{Ctx, Doc, Text}

trait CCharImpl[G] extends CCharOps[G] {
  this: CChar[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("char")
}
