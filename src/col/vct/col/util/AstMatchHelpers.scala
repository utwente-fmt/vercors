package vct.col.util

import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetIndex,
  CommTargetRange,
  CtExpr,
  Endpoint,
  Expr,
  RangeBinder,
}
import vct.col.origin.Origin
import vct.col.ref.Ref

object AstMatchHelpers {
  // Three helpers to provide aliases for the lengthy construction of CtExpr nodes
  object EndpointName {
    def unapply[G](expr: Expr[G]): Option[Ref[G, Endpoint[G]]] =
      expr match {
        case CtExpr(CommTargetEndpoint(endpoint)) => Some(endpoint)
        case _ => None
      }

    def apply[G](ref: Ref[G, Endpoint[G]])(implicit o: Origin): CtExpr[G] =
      CtExpr(CommTargetEndpoint(ref))
  }

  object EndpointRange {
    def unapply[G](
        expr: Expr[G]
    ): Option[(Ref[G, Endpoint[G]], RangeBinder[G])] =
      expr match {
        case CtExpr(CommTargetRange(ref, range)) => Some((ref, range))
        case _ => None
      }

    def apply[G](ref: Ref[G, Endpoint[G]], range: RangeBinder[G])(
        implicit o: Origin
    ): CtExpr[G] = CtExpr(CommTargetRange(ref, range))
  }

  object EndpointIndex {
    def unapply[G](expr: Expr[G]): Option[(Ref[G, Endpoint[G]], Expr[G])] =
      expr match {
        case CtExpr(CommTargetIndex(ref, index)) => Some((ref, index))
        case _ => None
      }

    def apply[G](ref: Ref[G, Endpoint[G]], index: Expr[G])(
        implicit o: Origin
    ): CtExpr[G] = CtExpr(CommTargetIndex(ref, index))
  }
}
