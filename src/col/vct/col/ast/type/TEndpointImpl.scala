package vct.col.ast.`type`

import vct.col.ast.TEndpoint
import vct.col.print._

trait TEndpointImpl[G] { this: TEndpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("endpoint")
}
