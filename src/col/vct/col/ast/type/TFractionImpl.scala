package vct.col.ast.`type`

import vct.col.ast.TFraction
import vct.col.print.{Ctx, Doc, Text}

trait TFractionImpl[G] {
  this: TFraction[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("frac")
}
