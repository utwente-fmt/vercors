package vct.col.ast.node

import vct.col.ast.{Applicable, Choreography, Declaration, Endpoint, Node}
import vct.col.check.{CheckContext, CheckError}
import vct.col.util.CurrentCheckNodeContext
import vct.result.VerificationError

trait NodeCheckOps[G] {
  this: Node[G] =>

  /* Check children first, so that the check of nodes higher in the tree may depend on the type and correctness of
    subnodes */
  final def checkTrans(context: CheckContext[G]): Seq[CheckError] = {
    val childrenErrors =
      checkContextRecursor(context, (ctx, node) => node.checkTrans(ctx)).flatten

    if (childrenErrors.nonEmpty) { childrenErrors }
    else {
      VerificationError.withContext(CurrentCheckNodeContext(this)) {
        check(context)
      }
    }
  }

  /** Applies a function to all subnodes, passing the appropriate context per
    * subnode. By default, all subnodes get the same context, as defined by
    * [[enterCheckContext]]
    * @param context
    *   the initial context
    * @param f
    *   the function to apply to all subnodes
    * @tparam T
    *   the result type of the function
    * @return
    *   the result of applying `f` with the appropriate context per subnode to
    *   all subnodes
    */
  def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] = subnodes.map(f(enterCheckContext(context), _))

  final def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    CheckContext(
      enterCheckContextScopes(context),
      enterCheckContextUndeclared(context),
      enterCheckContextRoScopes(context),
      enterCheckContextRoScopeReason(context),
      enterCheckContextCurrentApplicable(context),
      enterCheckContextInPreCondition(context),
      enterCheckContextInPostCondition(context),
      enterCheckContextCurrentChoreography(context),
      enterCheckContextCurrentReceiverEndpoint(context),
      enterCheckContextCurrentParticipatingEndpoints(context),
      enterCheckContextInChor(context),
      enterCheckContextInEndpointExpr(context),
      enterCheckContextDeclarationStack(context),
    )

  def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] = context.scopes
  def enterCheckContextUndeclared(
      context: CheckContext[G]
  ): Seq[Seq[Declaration[G]]] = context.undeclared
  def enterCheckContextRoScopes(context: CheckContext[G]): Int =
    context.roScopes
  def enterCheckContextRoScopeReason(
      context: CheckContext[G]
  ): Option[Node[G]] = context.roScopeReason
  def enterCheckContextCurrentApplicable(
      context: CheckContext[G]
  ): Option[Applicable[G]] = context.currentApplicable
  def enterCheckContextInPreCondition(context: CheckContext[G]): Boolean =
    context.inPreCondition
  def enterCheckContextInPostCondition(context: CheckContext[G]): Boolean =
    context.inPostCondition
  def enterCheckContextCurrentChoreography(
      context: CheckContext[G]
  ): Option[Choreography[G]] = context.currentChoreography
  def enterCheckContextCurrentReceiverEndpoint(
      context: CheckContext[G]
  ): Option[Endpoint[G]] = context.currentReceiverEndpoint
  def enterCheckContextCurrentParticipatingEndpoints(
      context: CheckContext[G]
  ): Option[Set[Endpoint[G]]] = context.currentParticipatingEndpoints
  def enterCheckContextInChor(context: CheckContext[G]): Boolean =
    context.inChor
  def enterCheckContextInEndpointExpr(
      context: CheckContext[G]
  ): Option[Endpoint[G]] = context.inEndpointExpr
  def enterCheckContextDeclarationStack(
      context: CheckContext[G]
  ): Seq[Declaration[G]] = context.declarationStack
}
