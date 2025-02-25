package vct.col.ast.`type`

import vct.col.ast.TRef
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TRefOps

trait TRefImpl[G] extends TRefOps[G] {
  this: TRef[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("ref")
}
