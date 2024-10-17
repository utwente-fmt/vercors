package vct.col.ast.family.location

import vct.col.ast.ByValueClassLocation
import vct.col.ast.ops.ByValueClassLocationOps
import vct.col.print.{Ctx, Doc}

trait ByValueClassLocationImpl[G] extends ByValueClassLocationOps[G] {
  this: ByValueClassLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show
}
