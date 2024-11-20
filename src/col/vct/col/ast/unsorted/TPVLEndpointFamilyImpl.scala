package vct.col.ast.unsorted

import vct.col.ast.TPVLEndpointFamily
import vct.col.ast.ops.TPVLEndpointFamilyOps
import vct.col.print._

trait TPVLEndpointFamilyImpl[G] extends TPVLEndpointFamilyOps[G] { this: TPVLEndpointFamily[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
