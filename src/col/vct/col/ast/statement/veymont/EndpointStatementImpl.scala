package vct.col.ast.statement.veymont

import vct.col.ast.{
  Assert,
  Assign,
  Assume,
  Block,
  Branch,
  CommTargetEndpoint,
  CommTargetRange,
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
  Node,
  RangeBinder,
  Scope,
  ThisChoreography,
  Variable,
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
        /* TODO (RR): In parameterized endpoint statements we want to restrict method calls to form
                      `nodes[i].m()`, where `i` has to be the iteration variable. But we ignore this
                      for now as automatically checking this is not so important yet.
                      For non-parameterized endpoint statements any form is allowed because VerCors
                      can easily compute the footprint.
         */
//        case _ => Seq(SeqProgInvocation(node))
        case _ => Seq()
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
        case Some(ct @ CommTargetEndpoint(_))
            if !context.currentParticipatingEndpoints.get.contains(ct) =>
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

  // Even though we use seq here, it's a bug if there's ever more than one (for example if also the inline range binder syntax is used)
  def rangeBinder: Seq[Variable[G]] =
    endpoint match {
      case Some(CommTargetRange(_, RangeBinder(v, _, _))) => Seq(v)
      case _ => Seq()
    }

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] = {
    // Add i to scope of the inner statement
    val ctxRangeBinder = context.copy(scopes = context.withScope(rangeBinder))
    endpoint.map(f(context, _)).toSeq ++ Seq(f(ctxRangeBinder, inner))
  }
}
