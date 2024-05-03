package vct.col.ast.statement.veymont

import vct.col.ast.{Assert, Assign, Assume, Block, Branch, ChorStatement, Communicate, CommunicateX, Deref, Endpoint, EndpointUse, Eval, Expr, Loop, MethodInvocation, Scope, SeqBranch, SeqLoop, ThisSeqProg, UnresolvedSeqBranch, UnresolvedSeqLoop, VeyMontAssignExpression}
import vct.col.ast.ops.ChorStatementOps
import vct.col.ast.statement.StatementImpl
import vct.col.check.{CheckContext, CheckError, SeqProgInvocation, SeqProgNoParticipant, SeqProgParticipant, SeqProgStatement}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ref.Ref

trait ChorStatementImpl[G] extends ChorStatementOps[G] with StatementImpl[G] { this: ChorStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("/* choreographic statement */") <+> inner.show

  object eval {
    def enterCheckContextCurrentReceiverEndpoint(chorStmt: ChorStatement[G], node: Eval[G], context: CheckContext[G]): Option[Endpoint[G]] =
      (chorStmt.endpoint, node) match {
        case (Some(Ref(endpoint)), Eval(MethodInvocation(_, _, _, _, _, _, _))) => Some(endpoint)
        case (None, Eval(MethodInvocation(EndpointUse(Ref(endpoint)), _, _, _, _, _, _))) => Some(endpoint)
        case _ => context.currentReceiverEndpoint
      }

    def check(chorStmt: ChorStatement[G], node: Eval[G], context: CheckContext[G]): Seq[CheckError] = (context.currentSeqProg, node.expr) match {
      case (None, _) => Seq()
      case (Some(_), MethodInvocation(EndpointUse(_), _, _, _, _, _, _)) => Seq()
      case (Some(_), MethodInvocation(ThisSeqProg(_), _, _, _, _, _, _)) => Seq()
      case _ => Seq(SeqProgInvocation(node))
    }
  }

  object assign {
    def root(expr: Expr[G]): Expr[G] = expr match {
      case MethodInvocation(e, _, _, _, _, _, _) => root(e)
      case Deref(obj, _) => root(obj)
      case e => e
    }

    def receiver(chorStmt: ChorStatement[G], node: Assign[G]): Option[Endpoint[G]] =
      chorStmt.endpoint.map(_.decl).orElse(root(node.target) match {
        case EndpointUse(Ref(endpoint)) => Some(endpoint)
        case _ => None
      })

    def enterCheckContextCurrentReceiverEndpoint(chorStmt: ChorStatement[G], node: Assign[G], context: CheckContext[G]): Option[Endpoint[G]] =
      receiver(chorStmt, node)

    def check(chorStmt: ChorStatement[G], node: Assign[G], context: CheckContext[G]): Seq[CheckError] = {
      receiver(chorStmt, node) match {
        case Some(endpoint) if !context.currentParticipatingEndpoints.get.contains(endpoint) =>
          Seq(SeqProgParticipant(chorStmt))
        case None =>
          Seq(SeqProgNoParticipant(chorStmt))
        case _ => Nil
      }
    }
  }

  override def enterCheckContextCurrentReceiverEndpoint(context: CheckContext[G]): Option[Endpoint[G]] = inner match {
    case node: Eval[G] => eval.enterCheckContextCurrentReceiverEndpoint(this, node, context)
    case node: Assign[G] => assign.enterCheckContextCurrentReceiverEndpoint(this, node, context)
    case _ => context.currentReceiverEndpoint
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = super.check(context) ++ (inner match {
    case node: Eval[G] => eval.check(this, node, context)
    case node: Assign[G] => assign.check(this, node, context)
    case
      _: CommunicateX[G] |
      _: Communicate[G] |
      _: VeyMontAssignExpression[G] |
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
    case _ => Seq(SeqProgStatement(this))
  })
}
