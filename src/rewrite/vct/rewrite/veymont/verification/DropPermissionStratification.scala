package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  ChorExpr,
  ChorPerm,
  EndpointExpr,
  EndpointStatement,
  Expr,
  Perm,
  Statement,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.rewrite.veymont.VeymontContext

object DropPermissionStratification extends RewriterBuilder {
  override def key: String = "dropPermissionStratification"
  override def desc: String =
    "Drops all stratified permission annotations, causing the program to be verified as if all endpoints can freely access all memory."
}

case class DropPermissionStratification[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case EndpointStatement(_, stat) => stat.rewriteDefault()
      case _ => statement.rewriteDefault()
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case EndpointExpr(_, expr) => expr.rewriteDefault()
      case ChorExpr(expr) => expr.rewriteDefault()
      case ChorPerm(_, loc, perm) =>
        Perm(loc.rewriteDefault(), perm.rewriteDefault())(expr.o)
      case _ => expr.rewriteDefault()
    }
}
