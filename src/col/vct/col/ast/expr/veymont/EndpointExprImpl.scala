package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.EndpointExprOps
import vct.col.ast.{Endpoint, EndpointExpr, Type}
import vct.col.check.{CheckContext, CheckError, EndpointExprInChor}
import vct.col.print._

trait EndpointExprImpl[G] extends EndpointExprOps[G] with ExprImpl[G] {
  this: EndpointExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("(\\[") <> ctx.name(endpoint) <> "]" <+> expr <> ")"
  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = expr.t

  override def enterCheckContextInEndpointExpr(
      context: CheckContext[G]
  ): Option[Endpoint[G]] = Some(this.endpoint.decl)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if (context.inChor)
      Seq(EndpointExprInChor(this))
    else
      Seq()
}
