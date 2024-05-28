package vct.col.ast.`type`

import vct.col.ast.TFraction
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TFractionOps

trait TFractionImpl[G] extends TFractionOps[G] {
  this: TFraction[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("frac")
}
