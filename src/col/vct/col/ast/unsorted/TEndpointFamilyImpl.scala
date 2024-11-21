package vct.col.ast.unsorted

import vct.col.ast.TEndpointFamily
import vct.col.ast.ops.TEndpointFamilyOps
import vct.col.print._

trait TEndpointFamilyImpl[G] extends TEndpointFamilyOps[G] {
  this: TEndpointFamily[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
