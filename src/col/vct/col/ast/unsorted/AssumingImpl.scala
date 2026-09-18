package vct.col.ast.unsorted

import vct.col.ast.{Assuming, Type}
import vct.col.print._
import vct.col.ast.ops.AssumingOps

trait AssumingImpl[G] extends AssumingOps[G] {
  this: Assuming[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  override def t: Type[G] = inner.t
}
