package vct.col.ast.statement.exceptional

import vct.col.ast.statement.StatementImpl
import vct.col.ast.{Deref, Endpoint, EndpointUse, Eval, Expr, MethodInvocation, PVLDeref, PVLEndpoint, PVLLocal, Statement, ThisSeqProg}
import vct.col.check.{CheckContext, CheckError, SeqProgInvocation}
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.EvalOps
import vct.col.ref.Ref
import vct.col.resolve.ctx.RefPVLEndpoint

trait EvalImpl[G] extends StatementImpl[G] with EvalOps[G] { this: Eval[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show <> ";"

  override def enterCheckContextCurrentReceiverEndpoint(context: CheckContext[G]): Option[Endpoint[G]] = this match {
    case Eval(MethodInvocation(expr, _, _, _, _, _, _)) if context.currentSeqProg.isDefined => root(expr) match {
      case EndpointUse(Ref(endpoint)) => Some(endpoint)
      case _ => context.currentReceiverEndpoint
    }
    case _ =>
      context.currentReceiverEndpoint
  }

  def root(expr: Expr[G]): Expr[G] = expr match {
    case MethodInvocation(e, _, _, _, _, _, _) => root(e)
    case Deref(obj, _) => root(obj)
    case e => e
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = super.check(context) ++ (context.currentSeqProg match {
    case None => Seq()
    case Some(_) => root(expr) match {
      case EndpointUse(_) => Seq()
      case ThisSeqProg(_) => Seq()
      case _ => Seq(SeqProgInvocation(this))
    }
  })
}