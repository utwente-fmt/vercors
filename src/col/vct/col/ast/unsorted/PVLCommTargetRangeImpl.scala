package vct.col.ast.unsorted

import vct.col.ast.{
  PVLCommTargetEndpoint,
  PVLCommTargetRange,
  RangeBinder,
  Type,
}
import vct.col.ast.ops.PVLCommTargetRangeOps
import vct.col.print._

trait PVLCommTargetRangeImpl[G] extends PVLCommTargetRangeOps[G] {
  this: PVLCommTargetRange[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  override def t: Type[G] = ref.get.decl.t
}
