package vct.col.ast.statement.veymont

import vct.col.ast.{
  Assert,
  Assign,
  Assume,
  Block,
  Branch,
  Communicate,
  CommunicateStatement,
  CommunicateTarget,
  CtExpr,
  Deref,
  Endpoint,
  EndpointStatement,
  Eval,
  Expr,
  Loop,
  MethodInvocation,
  Scope,
  ThisChoreography,
}
import vct.col.ast.ops.EndpointStatementOps
import vct.col.ast.statement.StatementImpl
import vct.col.check.{
  CheckContext,
  CheckError,
  ChorStatement,
  SeqProgInvocation,
  SeqProgNoParticipant,
  SeqProgParticipant,
}
import vct.col.print.{Ctx, Doc, Line, Text}
import vct.col.ref.Ref
import vct.col.util.AstMatchHelpers.{EndpointIndex, EndpointName, EndpointRange}

trait EndpointStatementImpl[G]
    extends EndpointStatementOps[G] with StatementImpl[G] {
  this: EndpointStatement[G] =>
  assert(wellformed)
  def wellformed: Boolean =
    inner match {
      // There are only a few statements where we fully define how projection works - for now
      case _: Assign[G] | _: Assert[G] | _: Eval[G] => true
      case _ => false
    }

  override def layout(implicit ctx: Ctx): Doc =
    (endpoint match {
      case Some(commTarget) =>
        Text(s"\\\\endpoint_statement") <+> commTarget <> ";"
      case None => Text("\\\\unlabeled_endpoint_statement")
    }) <+> inner

  // TODO (RR): Should go through the routines below once more. There might actually be subtle bugs lurking there, maybe these should all just be refactored to be separate nodes, instead of reusing col core nodes

  object eval {
    def enterCheckContextCurrentReceiverEndpoint(
        chorStmt: EndpointStatement[G],
        node: Eval[G],
        context: CheckContext[G],
    ): Option[CommunicateTarget[G]] =
      (chorStmt.endpoint, node) match {
        case (Some(commTarget), Eval(MethodInvocation(_, _, _, _, _, _, _))) =>
          Some(commTarget)
        case (None, Eval(MethodInvocation(e, _, _, _, _, _, _)))
            if rootEndpoint(e).isDefined =>
          Some(rootEndpoint(e).get)
        case _ => context.currentReceiverEndpoint
      }

    def check(
        chorStmt: EndpointStatement[G],
        node: Eval[G],
        context: CheckContext[G],
    ): Seq[CheckError] =
      (context.currentChoreography, node.expr) match {
        case (None, _) => Seq()
        case (
              Some(_),
              MethodInvocation(ThisChoreography(_), _, _, _, _, _, _),
            ) =>
          Seq()
        case (Some(_), MethodInvocation(e, _, _, _, _, _, _))
            if rootEndpoint(e).isDefined =>
          Seq()
        case _ => Seq(SeqProgInvocation(node))
      }
  }

  def rootEndpoint(expr: Expr[G]): Option[CommunicateTarget[G]] =
    expr match {
      case MethodInvocation(e, _, _, _, _, _, _) => rootEndpoint(e)
      case Deref(obj, _) => rootEndpoint(obj)
      case CtExpr(target) => Some(target)
      case _ => None
    }

  object assign {
    def receiver(
        chorStmt: EndpointStatement[G],
        node: Assign[G],
    ): Option[CommunicateTarget[G]] = chorStmt.endpoint

    def enterCheckContextCurrentReceiverEndpoint(
        chorStmt: EndpointStatement[G],
        node: Assign[G],
        context: CheckContext[G],
    ): Option[CommunicateTarget[G]] = receiver(chorStmt, node)

    def check(
        chorStmt: EndpointStatement[G],
        node: Assign[G],
        context: CheckContext[G],
    ): Seq[CheckError] = {
      receiver(chorStmt, node) match {
        case Some(endpoint)
            if !context.currentParticipatingEndpoints.get.contains(endpoint) =>
          Seq(SeqProgParticipant(chorStmt))
        case _ => Nil
      }
    }
  }

  override def enterCheckContextCurrentReceiverEndpoint(
      context: CheckContext[G]
  ): Option[CommunicateTarget[G]] =
    inner match {
      case node: Eval[G] =>
        eval.enterCheckContextCurrentReceiverEndpoint(this, node, context)
      case node: Assign[G] =>
        assign.enterCheckContextCurrentReceiverEndpoint(this, node, context)
      case _ => context.currentReceiverEndpoint
    }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (inner match {
        case node: Eval[G] => eval.check(this, node, context)
        case node: Assign[G] => assign.check(this, node, context)
        case _: CommunicateStatement[G] | _: Branch[G] | _: Loop[G] |
            _: Scope[G] | _: Block[G] | _: Assert[G] | _: Assume[G] |
            _: EndpointStatement[G] =>
          Seq()
        case _ => Seq(ChorStatement(this))
      })
}
