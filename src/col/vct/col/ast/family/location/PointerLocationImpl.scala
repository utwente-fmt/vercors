package vct.col.ast.family.location

import vct.col.ast.PointerLocation
import vct.col.print.{Ctx, Doc}

trait PointerLocationImpl[G] {
  this: PointerLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc = pointer.show
}
