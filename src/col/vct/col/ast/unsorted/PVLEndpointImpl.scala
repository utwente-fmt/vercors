package vct.col.ast.unsorted

import vct.col.ast.PVLEndpoint
import vct.col.ast.ops.PVLEndpointOps
import vct.col.print._

trait PVLEndpointImpl[G] extends PVLEndpointOps[G] { this: PVLEndpoint[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
