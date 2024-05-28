package vct.col.ast.`type`

import vct.col.ast.TNothing
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TNothingOps

trait TNothingImpl[G] extends TNothingOps[G] {
  this: TNothing[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("nothing")
}
