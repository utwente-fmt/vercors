package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.EndpointExprOps
import vct.col.ast.{Endpoint, EndpointExpr, TBool, TResource, Type}
import vct.col.check.{CheckContext, CheckError, InconsistentEndpointExprNesting}
import vct.col.print._

trait EndpointExprImpl[G] extends EndpointExprOps[G] with ExprImpl[G] {
  this: EndpointExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("(\\[") <> ctx.name(endpoint) <> "]" <+> expr <> ")"
  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = expr.t

  override def enterCheckContextInEndpointExpr(
      context: CheckContext[G]
  ): Option[EndpointExpr[G]] = Some(this)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (context.inEndpointExpr match {
        case Some(endpointExpr)
            if endpointExpr.endpoint.decl != this.endpoint.decl =>
          Seq(InconsistentEndpointExprNesting(endpointExpr, this))
        case _ => Seq()
      })
}
