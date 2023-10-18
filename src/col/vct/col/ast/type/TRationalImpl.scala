package vct.col.ast.`type`

import vct.col.ast.TRational
import vct.col.print.{Ctx, Doc, Text}

trait TRationalImpl[G] {
  this: TRational[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("rational")
}
