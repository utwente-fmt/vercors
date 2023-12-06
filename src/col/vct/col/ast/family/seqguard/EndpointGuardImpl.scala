package vct.col.ast.family.seqguard

import vct.col.ast.EndpointGuard

trait EndpointGuardImpl[G] { this: EndpointGuard[G] =>
  def endpointOpt = Some(endpoint.decl)
}
