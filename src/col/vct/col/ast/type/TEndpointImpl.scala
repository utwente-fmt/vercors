package vct.col.ast.`type`

import vct.col.ast.TEndpoint
import vct.col.print._
import vct.col.ast.ops.TEndpointOps

trait TEndpointImpl[G] extends TEndpointOps[G] {
  this: TEndpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("endpoint")
}
