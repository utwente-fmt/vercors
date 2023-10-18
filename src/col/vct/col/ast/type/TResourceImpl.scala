package vct.col.ast.`type`

import vct.col.ast.TResource
import vct.col.print.{Ctx, Doc, Text}

trait TResourceImpl[G] {
  this: TResource[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("resource")
}
