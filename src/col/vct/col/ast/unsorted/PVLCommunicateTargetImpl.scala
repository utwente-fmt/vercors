package vct.col.ast.unsorted

import vct.col.ast.{
  PVLCommTargetEndpoint,
  PVLCommTargetIndex,
  PVLCommTargetRange,
  PVLCommunicateTarget,
  Type,
}
import vct.col.ast.ops.PVLCommunicateTargetFamilyOps
import vct.col.print._

trait PVLCommunicateTargetImpl[G] extends PVLCommunicateTargetFamilyOps[G] {
  this: PVLCommunicateTarget[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def asName: PVLCommTargetEndpoint[G] = {
    require(this.isInstanceOf[PVLCommTargetEndpoint[G]])
    this.asInstanceOf[PVLCommTargetEndpoint[G]]
  }

  def asRange: PVLCommTargetRange[G] = {
    require(this.isInstanceOf[PVLCommTargetRange[G]])
    this.asInstanceOf[PVLCommTargetRange[G]]
  }

  def asIndex: PVLCommTargetIndex[G] = {
    require(this.isInstanceOf[PVLCommTargetIndex[G]])
    this.asInstanceOf[PVLCommTargetIndex[G]]
  }

  def t: Type[G]

}
