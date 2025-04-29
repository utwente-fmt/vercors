package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.EndpointExprOps
import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetIndex,
  CommTargetRange,
  Declaration,
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
    Text("(\\endpoint") <+> endpoint <> forallPart <> ";" <+> expr <> ")"
  override def precedence: Int = Precedence.ATOMIC

  def forallPart(implicit ctx: Ctx): Doc =
    bindings match {
      case Seq() => Empty
      case bindings => Text(", ∀") <> Doc.args(bindings.map(_.show))
    }

  override def t: Type[G] = expr.t

  override def enterCheckContextInEndpointExpr(
      context: CheckContext[G]
  ): Option[EndpointExpr[G]] = Some(this)

  def declarations: Seq[Declaration[G]] =
    ((endpoint match {
      case CommTargetRange(_, range) => Seq(range.binder)
      case _ => Seq()
    }) ++ bindings)

  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] =
    declarations match {
      case Seq() => context.scopes
      case decls => context.withScope(decls)
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
