package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.ChorExprOps
import vct.col.ast.{ChorExpr, Type}
import vct.col.check.{CheckContext, CheckError, ChorInEndpointExpr}
import vct.col.print._

trait ChorExprImpl[G] extends ChorExprOps[G] with ExprImpl[G] {
  this: ChorExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("(\\chor") <+> expr <> ")"
  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = expr.t

  override def enterCheckContextInChor(context: CheckContext[G]): Boolean = true

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if (context.inEndpointExpr.nonEmpty)
      Seq(ChorInEndpointExpr(this))
    else
      Seq()
}
