package vct.col.ast.unsorted

import vct.col.ast.{Asserting, Type}
import vct.col.print._
import vct.col.ast.ops.AssertingOps

trait AssertingImpl[G] extends AssertingOps[G] {
  this: Asserting[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  override def t: Type[G] = inner.t
}
