package vct.col.ast.unsorted

import vct.col.ast.{RangeBinder, TInt}
import vct.col.ast.ops.RangeBinderOps
import vct.col.print._
import vct.col.ast.ops.{RangeBinderFamilyOps, RangeBinderOps}

trait RangeBinderImpl[G]
    extends RangeBinderOps[G] with RangeBinderFamilyOps[G] {
  this: RangeBinder[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  require(binder.t == TInt[G]())
}
