package vct.rewrite.veymont

import vct.col.ast.{AnyFunctionInvocation, ChorExpr, Expr, Program}
import vct.col.rewrite.{
  Generation,
  Rewriter,
  RewriterBuilder,
  RewriterBuilderArg,
}
import vct.col.util.AstBuildHelpers._

object PushInChor extends RewriterBuilderArg[Boolean] {
  override def key: String = "pushInChor"

  override def desc: String =
    "Pushes in `\\chor` expressions when generating permissions. This simplifies later analysis to be done by encodePermissionStratification."
}

case class PushInChor[Pre <: Generation](veymontGeneratePermissions: Boolean)
    extends Rewriter[Pre] {

  case class IdentityRewriter[Pre <: Generation]() extends Rewriter[Pre] {}

  override def dispatch(program: Program[Pre]): Program[Post] =
    if (veymontGeneratePermissions)
      program.rewriteDefault()
    else
      IdentityRewriter().dispatch(program)

  def containsFunctionInvocation(expr: Expr[Pre]): Boolean =
    expr.transSubnodes.exists {
      case inv: AnyFunctionInvocation[Pre] => true
      case _ => false
    }

  def countEndpoints(expr: Expr[Pre]): Int =
    InferEndpointContexts.getEndpoints(expr).size

  def needsChor(expr: Expr[Pre]): Boolean =
    containsFunctionInvocation(expr) || countEndpoints(expr) > 1

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case ChorExpr(inner) =>
        implicit val o = expr.o
        foldAny(expr.t)(unfoldStar(inner).map {
          case expr if needsChor(expr) => ChorExpr(dispatch(expr))(expr.o)
          case expr => dispatch(expr)
        })
      case _ => expr.rewriteDefault()
    }
}
