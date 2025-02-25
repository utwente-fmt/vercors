package vct.col.ast.`type`

import vct.col.ast.TResource
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TResourceOps

trait TResourceImpl[G] extends TResourceOps[G] {
  this: TResource[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("resource")
}
