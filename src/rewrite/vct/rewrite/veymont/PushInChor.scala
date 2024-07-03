package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{
  AnyFunctionInvocation,
  ChorExpr,
  Communicate,
  Declaration,
  Expr,
  Program,
  Statement,
}
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

  val inInvariant = ScopedStack[Boolean]()

  override def dispatch(program: Program[Pre]): Program[Post] =
    if (veymontGeneratePermissions)
      program.rewriteDefault()
    else
      IdentityRewriter().dispatch(program)

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case comm: Communicate[Pre] =>
        comm.rewrite(invariant =
          inInvariant.having(true) { comm.invariant.rewriteDefault() }
        ).succeed(comm)
      case _ => super.dispatch(decl)
    }

  def containsFunctionInvocation(expr: Expr[Pre]): Boolean =
    expr.exists {
      case inv: AnyFunctionInvocation[Pre] => true
      case _ => false
    }

  def countEndpoints(expr: Expr[Pre]): Int =
    InferEndpointContexts.getEndpoints(expr).size

  // Only keep chor in two cases: when there's a function or when there's more than 1 endpoint.
  // For functions: functions might reference multiple endpoints. Therefore, all fields should be unwrapped
  // before it is evaluated.
  // For > 1 endpoints: Multiple endpoint contexts are mixed, which means StratifyExprssions will not put it in an
  // EndpointExpr node. Therefore, the \chor needs to be kept such that all field permissions are probably unfolded
  // and the inner expression is safely evaluated.
  // If neither is the case, then it is safe to remove the \chor expression. StratifyExpressions will then wrap the
  // expression in an EndpointExpr, which will in turn cause read functions to be used by EncodePermissionStratification
  // This doesn't make analysis more powerful, but it might save a chain of unfoldings here and there.
  // In addition, this optimization is only possible when generating permissions, as indicated by the
  // veymontGeneratePermissions parameter, because only then we know the owner of each field, which is what makes
  // wrapping exprs in EndpointExpr's safe: they explicitly model the semantics of permission generation.
  def needsChor(expr: Expr[Pre]): Boolean =
    containsFunctionInvocation(expr) || countEndpoints(expr) > 1

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case ChorExpr(_) if inInvariant.topOption.contains(true) =>
        expr.rewriteDefault()
      case ChorExpr(inner) =>
        implicit val o = expr.o
        foldAny(expr.t)(unfoldStar(inner).map {
          case expr if needsChor(expr) => ChorExpr(dispatch(expr))(expr.o)
          case expr => dispatch(expr)
        })
      case _ => expr.rewriteDefault()
    }
}
