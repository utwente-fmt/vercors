package vct.col.ast.family.seqguard

import vct.col.ast.EndpointGuard
import vct.col.ast.ops.EndpointGuardOps

trait EndpointGuardImpl[G] extends EndpointGuardOps[G] { this: EndpointGuard[G] =>
  def endpointOpt = Some(endpoint.decl)
}
