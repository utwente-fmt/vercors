package vct.col.ast.unsorted

import vct.col.ast.{Expr, PVLChorStatement, PVLDeref, PVLEndpoint, PVLLocal, Assign}
import vct.col.ast.ops.PVLChorStatementOps
import vct.col.check.{CheckContext, CheckError, PVLSeqAssignEndpoint}
import vct.col.print._
import vct.col.resolve.ctx.RefPVLEndpoint

trait PVLChorStatementImpl[G] extends PVLChorStatementOps[G] { this: PVLChorStatement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  object assign {
    def apply(): Assign[G] = inner.asInstanceOf[Assign[G]]
    def get: Option[Assign[G]] = inner match {
      case assign: Assign[G] => Some(assign)
      case _ => None
    }

    def target: Expr[G] = assign().target

    def endpoint: Option[PVLEndpoint[G]] = endpointHelper(target)

    def endpointHelper(expr: Expr[G]): Option[PVLEndpoint[G]] = expr match {
      case PVLDeref(obj, _) => endpointHelper(obj)
      case local: PVLLocal[G] => local.ref match {
        case Some(RefPVLEndpoint(endpoint)) => Some(endpoint)
        case _ => None
      }
      case _ => None
    }
  }

  // TODO (RR): How to redo this error? Purely with permissions? Then it has to be a blame
//  override def check(context: CheckContext[G]): Seq[CheckError] = super.check(context) ++ (endpoint match {
//    case None => Seq(PVLSeqAssignEndpoint(this))
//    case Some(_) => Seq()
//  })
}
