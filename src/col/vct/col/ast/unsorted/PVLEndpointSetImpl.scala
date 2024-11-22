package vct.col.ast.unsorted

import vct.col.ast.{PVLEndpointName, PVLEndpointRange, PVLEndpointSet}
import vct.col.ast.ops.PVLEndpointSetFamilyOps
import vct.col.print._

trait PVLEndpointSetImpl[G] extends PVLEndpointSetFamilyOps[G] {
  this: PVLEndpointSet[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def asName: PVLEndpointName[G] = {
    require(this.isInstanceOf[PVLEndpointName[G]])
    this.asInstanceOf[PVLEndpointName[G]]
  }

  def asRange: PVLEndpointRange[G] = {
    require(this.isInstanceOf[PVLEndpointRange[G]])
    this.asInstanceOf[PVLEndpointRange[G]]
  }

}
