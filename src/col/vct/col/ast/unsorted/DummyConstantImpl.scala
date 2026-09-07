package vct.col.ast.unsorted

import vct.col.ast.DummyConstant
import vct.col.ast.ops.DummyConstantOps
import vct.col.print._

trait DummyConstantImpl[G] extends DummyConstantOps[G] {
  this: DummyConstant[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
