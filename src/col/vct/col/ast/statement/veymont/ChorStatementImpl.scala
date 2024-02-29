package vct.col.ast.statement.veymont

import vct.col.ast.{Assert, Assign, Assume, Block, Branch, ChorStatement, Communicate, CommunicateX, Deref, Endpoint, EndpointUse, Eval, Expr, Loop, MethodInvocation, Scope, SeqAssign, SeqBranch, SeqLoop, ThisSeqProg, UnresolvedSeqBranch, UnresolvedSeqLoop, VeyMontAssignExpression}
import vct.col.ast.ops.ChorStatementOps
import vct.col.ast.statement.StatementImpl
import vct.col.check.{CheckContext, CheckError, SeqProgInvocation, SeqProgStatement}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ref.Ref

trait ChorStatementImpl[G] extends ChorStatementOps[G] with StatementImpl[G] { this: ChorStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc =
  Text("/* choreographic statement */") <+> inner.show

  override def enterCheckContextCurrentReceiverEndpoint(context: CheckContext[G]): Option[Endpoint[G]] = inner match {
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

  override def check(context: CheckContext[G]): Seq[CheckError] = super.check(context) ++ (inner match {
    case
      _: CommunicateX[G] |
      _: Communicate[G] |
      _: VeyMontAssignExpression[G] |
      _: Assign[G] |
      _: SeqAssign[G] |
      _: Branch[G] |
      _: Loop[G] |
      _: Scope[G] |
      _: Block[G] |
      _: Assert[G] |
      _: Assume[G] |
      _: UnresolvedSeqBranch[G] |
      _: UnresolvedSeqLoop[G] |
      _: SeqBranch[G] |
      _: SeqLoop[G] |
      _: ChorStatement[G] => Seq()
    case Eval(inv: MethodInvocation[G]) => root(inv.obj) match {
      case EndpointUse(_) => Seq()
      case ThisSeqProg(_) => Seq()
      case _ => Seq(SeqProgInvocation(inner))
    }
    case _ => Seq(SeqProgStatement(this))
  })
}
