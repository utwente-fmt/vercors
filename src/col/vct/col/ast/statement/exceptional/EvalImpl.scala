package vct.col.ast.statement.exceptional

import vct.col.ast.{EndpointUse, Eval, MethodInvocation, Statement, ThisSeqProg}
import vct.col.check.{CheckContext, CheckError, SeqProgInvocation}
import vct.col.print.{Ctx, Doc}

trait EvalImpl[G] { this: Eval[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show <> ";"

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] = this match {
    case Eval(MethodInvocation(EndpointUse(endpoint), _, _, _, _, _, _)) =>
      context.withReceiverEndpoint(endpoint.decl)
    case _ => context
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = this.asInstanceOf[Statement[G]].check(context) ++ (context.currentSeqProg match {
    case None => Seq()
    case Some(_) => this.expr match {
      case MethodInvocation(EndpointUse(_), _, _, _, _, _, _) => Seq()
      case MethodInvocation(ThisSeqProg(_), _, _, _, _, _, _) => Seq()
      case _ => Seq(SeqProgInvocation(this))
    }
  })
}