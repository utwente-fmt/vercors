package vct.col.ast.statement.exceptional

import vct.col.ast.statement.StatementImpl
import vct.col.ast.{Endpoint, EndpointUse, Eval, MethodInvocation, Statement, ThisSeqProg}
import vct.col.check.{CheckContext, CheckError, SeqProgInvocation}
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.EvalOps

trait EvalImpl[G] extends StatementImpl[G] with EvalOps[G] { this: Eval[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show <> ";"

  override def enterCheckContextCurrentReceiverEndpoint(context: CheckContext[G]): Option[Endpoint[G]] = this match {
    case Eval(MethodInvocation(EndpointUse(endpoint), _, _, _, _, _, _)) if context.currentSeqProg.isDefined =>
      Some(endpoint.decl)
    case _ =>
      context.currentReceiverEndpoint
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = super.check(context) ++ (context.currentSeqProg match {
    case None => Seq()
    case Some(_) => this.expr match {
      case MethodInvocation(EndpointUse(_), _, _, _, _, _, _) => Seq()
      case MethodInvocation(ThisSeqProg(_), _, _, _, _, _, _) => Seq()
      case _ => Seq(SeqProgInvocation(this))
    }
  })
}