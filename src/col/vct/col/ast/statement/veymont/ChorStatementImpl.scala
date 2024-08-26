package vct.col.ast.statement.veymont

import vct.col.ast.ops.ChorStatementOps
import vct.col.ast.statement.StatementImpl
import vct.col.ast._
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers.unfoldStar

import scala.collection.immutable.ListSet

trait ChorStatementImpl[G] extends ChorStatementOps[G] with StatementImpl[G] {
  this: ChorStatement[G] =>

  // use non-existent syntax to be explicit about presence of an internal node
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\\\chor_statement") <+/> inner

  def exprs: Seq[Expr[G]] =
    (inner match {
      case branch: Branch[G] => branch.branches.map(_._1)
      case loop: Loop[G] => Seq(loop.cond)
      case assert: Assert[G] => Seq(assert.res)
      case assume: Assume[G] => Seq(assume.assn)
      case inhale: Inhale[G] => Seq(inhale.res)
      case exhale: Exhale[G] => Seq(exhale.res)
    }).flatMap(unfoldStar)

  // All explicitly mentioned endpoints in the relevant expressions.
  // Note that this is an underapproximation of the actual participating endpoints,
  // in the case of unpointed expressions, meaning expressions in e.g. a branch without \endpoint.
  // These expressions are simply checked by _all_ participating endpoints, plus any
  // explicitly mentioned endpoints.
  def explicitEndpoints: Seq[Endpoint[G]] =
    exprs.collect { case EndpointExpr(Ref(endpoint), _) => endpoint }

  def hasUnpointed: Boolean =
    exprs.exists {
      case _: EndpointExpr[G] => false
      case _ => true
    }

  def loopOrBranch: Boolean =
    inner match {
      case _: Branch[G] | _: Loop[G] => true
      case _ => false
    }

  def allowed: Boolean =
    inner match {
      case _: Branch[G] | _: Loop[G] | _: Assert[G] | _: Inhale[G] |
          _: Exhale[G] | _: Assume[G] =>
        true
      case _ => false
    }

  def participants: Set[Endpoint[G]] =
    ListSet.from(collect {
      case comm: Communicate[G] => comm.participants
      case EndpointStatement(Some(Ref(endpoint)), Assign(_, _)) => Seq(endpoint)
      case c @ ChorStatement(_) => c.explicitEndpoints
    }.flatten)

  // There are only a few statements where we fully define how projection works
  def allowedInner: Option[CheckError] =
    Option.when(!allowed)(vct.col.check.ChorStatement(this))

  // There are participants in this if that have been excluded from participation: error
  def participantCheck(context: CheckContext[G]): Option[CheckError] =
    if (
      !Set.from(explicitEndpoints)
        .subsetOf(context.currentParticipatingEndpoints.get)
    )
      Some(SeqProgParticipant(this))
    else
      None

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    assert(context.currentParticipatingEndpoints.isDefined)
    super.check(context) ++ allowedInner.toSeq ++
      (if (loopOrBranch)
         participantCheck(context).toSeq
       else
         Seq())
  }

  override def enterCheckContextCurrentParticipatingEndpoints(
      context: CheckContext[G]
  ): Option[Set[Endpoint[G]]] =
    if (loopOrBranch) {
      // Assume SeqProg sets participatingEndpoints
      assert(context.currentParticipatingEndpoints.isDefined)

      if (hasUnpointed) {
        // Everyone that is participating keeps participating, as well as any endpoints explicitly mentioned
        context.appendCurrentParticipatingEndpoints(explicitEndpoints)
      } else {
        // We can refine the set of participants down to the set of endpoints actually present in the guard
        context.withCurrentParticipatingEndpoints(explicitEndpoints)
      }
    } else { context.currentParticipatingEndpoints }
}
