package vct.col.ast.statement.veymont

import vct.col.ast.{Deref, Expr, PVLDeref, PVLEndpoint, PVLLocal, PVLSeqAssign}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.PVLSeqAssignOps
import vct.col.ast.statement.StatementImpl
import vct.col.check.{CheckContext, CheckError, PVLSeqAssignEndpoint}
import vct.col.resolve.ctx.RefPVLEndpoint

trait  PVLSeqAssignImpl[G] extends PVLSeqAssignOps[G] with StatementImpl[G] { this: PVLSeqAssign[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(expr.show <+> ":=" <+> value.show)

  def endpoint: Option[PVLEndpoint[G]] = endpointHelper(expr)

  def endpointHelper(expr: Expr[G]): Option[PVLEndpoint[G]] = expr match {
    case PVLDeref(obj, _) => endpointHelper(obj)
    case local: PVLLocal[G] => local.ref match {
      case Some(RefPVLEndpoint(endpoint)) => Some(endpoint)
      case _ => None
    }
    case _ => None
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = super.check(context) ++ (endpoint match {
    case None => Seq(PVLSeqAssignEndpoint(this))
    case Some(_) => Seq()
  })
}
