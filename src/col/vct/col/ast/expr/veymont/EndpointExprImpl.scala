package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.EndpointExprOps
import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetIndex,
  CommTargetRange,
  Endpoint,
  EndpointExpr,
  TBool,
  TResource,
  Type,
}
import vct.col.check.{CheckContext, CheckError, InconsistentEndpointExprNesting}
import vct.col.print._

trait EndpointExprImpl[G] extends EndpointExprOps[G] with ExprImpl[G] {
  this: EndpointExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> "\\endpoint" <+> endpoint <> ";" <+> expr <> ")"
  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = expr.t

  override def enterCheckContextInEndpointExpr(
      context: CheckContext[G]
  ): Option[EndpointExpr[G]] = Some(this)

  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    endpoint match {
      case CommTargetRange(ref, range) => context.withScope(Seq(range.binder))
      case _ => context.scopes
    }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (context.inEndpointExpr match {
        // It has to be syntactically the same endpoint expr, otherwise you can't nest it
        // You could do refinement in theory but that's way out of scope for now
        case Some(endpointExpr) if endpointExpr.endpoint != this.endpoint =>
          Seq(InconsistentEndpointExprNesting(endpointExpr, this))
        case _ => Seq()
      })
}
