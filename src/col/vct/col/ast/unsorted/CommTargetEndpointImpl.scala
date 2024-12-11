package vct.col.ast.unsorted

import vct.col.ast.node.NodeImpl
import vct.col.ast.{CommTargetEndpoint, Endpoint}
import vct.col.ast.ops.CommTargetEndpointOps
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._

trait CommTargetEndpointImpl[G]
    extends CommTargetEndpointOps[G] with CommunicateTargetImpl[G] {
  this: CommTargetEndpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))

  def endpoint: Endpoint[G] = ref.decl

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ {
      // This is really a well-formedness requirement on the AST. Ideally it would be checked earlier, but this is not
      // possibly, as the ref might not be resolved yet.
      require(endpoint.isSingle)
      Seq()
    }
}
