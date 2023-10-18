package vct.col.ast.`type`

import vct.col.ast.TProcess
import vct.col.print.{Ctx, Doc, Text}

trait TProcessImpl[G] {
  this: TProcess[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("process")
}
