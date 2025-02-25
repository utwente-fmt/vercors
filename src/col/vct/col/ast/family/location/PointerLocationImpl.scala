package vct.col.ast.family.location

import vct.col.ast.PointerLocation
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.PointerLocationOps

trait PointerLocationImpl[G] extends PointerLocationOps[G] {
  this: PointerLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc = pointer.show
}
