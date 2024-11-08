package vct.col.ast.unsorted

import vct.col.ast.PVLEndpointSet
import vct.col.ast.ops.PVLEndpointSetFamilyOps
import vct.col.print._

trait PVLEndpointSetImpl[G] extends PVLEndpointSetFamilyOps[G] { this: PVLEndpointSet[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
