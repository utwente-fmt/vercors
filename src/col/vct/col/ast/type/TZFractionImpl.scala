package vct.col.ast.`type`

import vct.col.ast.TZFraction
import vct.col.print.{Ctx, Doc, Text}

trait TZFractionImpl[G] { this: TZFraction[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("zfrac")
}