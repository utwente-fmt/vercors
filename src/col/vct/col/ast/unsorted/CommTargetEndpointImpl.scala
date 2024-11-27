package vct.col.ast.unsorted

import vct.col.ast.{CommTargetEndpoint, Endpoint}
import vct.col.ast.ops.CommTargetEndpointOps
import vct.col.print._

trait CommTargetEndpointImpl[G] extends CommTargetEndpointOps[G] {
  this: CommTargetEndpoint[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def endpoint: Endpoint[G] = ref.decl
}
