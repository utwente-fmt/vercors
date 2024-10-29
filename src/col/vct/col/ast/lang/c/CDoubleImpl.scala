package vct.col.ast.lang.c

import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.CDouble
import vct.col.ast.ops.CDoubleOps

trait CDoubleImpl[G] extends CDoubleOps[G] {
  this: CDouble[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("double")
}
