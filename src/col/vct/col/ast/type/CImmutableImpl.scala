package vct.col.ast.`type`

import vct.col.ast.CImmutable
import vct.col.ast.ops.CImmutableOps
import vct.col.print.{Ctx, Doc, Text}

trait CImmutableImpl[G] extends CImmutableOps[G] {
  this: CImmutable[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("immutable")
}
