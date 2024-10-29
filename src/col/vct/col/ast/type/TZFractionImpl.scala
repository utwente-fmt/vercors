package vct.col.ast.`type`

import vct.col.ast.TZFraction
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TZFractionOps

trait TZFractionImpl[G] extends TZFractionOps[G] {
  this: TZFraction[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("zfrac")
}
