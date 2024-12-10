package vct.col.ast.unsorted

import vct.col.ast.{PVLCommTargetEndpoint, PVLCommTargetIndex, Type}
import vct.col.ast.ops.PVLCommTargetIndexOps
import vct.col.print._

trait PVLCommTargetIndexImpl[G] extends PVLCommTargetIndexOps[G] {
  this: PVLCommTargetIndex[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  override def t: Type[G] = ref.get.decl.t
}
