package vct.col.ast.lang.pvl

import vct.col.ast.ops.PVLEndpointStatementOps
import vct.col.ast._
import vct.col.resolve.ctx.RefPVLEndpoint

trait PVLEndpointStatementImpl[G] extends PVLEndpointStatementOps[G] {
  this: PVLEndpointStatement[G] =>
  assert(!inner.isInstanceOf[EndpointStatement[_]])

  object assign {
    def apply(): Assign[G] = inner.asInstanceOf[Assign[G]]
    def get: Option[Assign[G]] =
      inner match {
        case assign: Assign[G] => Some(assign)
        case _ => None
      }

    def target: Expr[G] = assign().target

    def endpoint: Option[PVLEndpoint[G]] = endpointHelper(target)

    def endpointHelper(expr: Expr[G]): Option[PVLEndpoint[G]] =
      expr match {
        case PVLDeref(obj, _) => endpointHelper(obj)
        case local: PVLLocal[G] =>
          local.ref match {
            case Some(RefPVLEndpoint(endpoint)) => Some(endpoint)
            case _ => None
          }
        case _ => None
      }
  }
}
