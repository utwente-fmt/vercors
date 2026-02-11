package vct.col.ast.`type`

import vct.col.ast.TImmutable
import vct.col.ast.ops.TImmutableOps
import vct.col.print._

trait TImmutableImpl[G] extends TImmutableOps[G] {
  this: TImmutable[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("immutable") <+> inner
}
