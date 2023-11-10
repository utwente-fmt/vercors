package vct.col.ast.statement.veymont

import vct.col.ast.{Endpoint, EndpointGuard, SeqBranch, UnpointedGuard}
import vct.col.ast.statement.StatementImpl
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.ref.Ref

trait SeqBranchImpl[G] extends StatementImpl[G] { this: SeqBranch[G] =>
  def hasUnpointed: Boolean = guards.exists { case _: UnpointedGuard[G] => true; case _ => false }
  def explicitParticipants: Seq[Endpoint[G]] = guards.collect { case EndpointGuard(Ref(endpoint), condition) => endpoint }

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] = {
    // Assume SeqProg sets participatingEndpoints
    assert(context.currentParticipatingEndpoints.isDefined)

    if (hasUnpointed) {
      // Everyone that is participating keeps participating, as well as any endpoints explicitly mentioned
      context.appendCurrentParticipatingEndpoints(explicitParticipants)
    } else {
      // We can refine the set of participants down to the set of endpoints actually present in the guard
      context.withCurrentParticipatingEndpoints(explicitParticipants)
    }
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = super.check(context) ++ {
    // Assume SeqProg sets participatingEndpoints
    assert(context.currentParticipatingEndpoints.isDefined)

    // Ensure the set of participants is at most refined
    if (Set.from(explicitParticipants).subsetOf(context.currentParticipatingEndpoints.get)) {
      Seq()
    } else {
      // There are participants in this if that have been excluded from participation: error
      Seq(SeqProgParticipant(this))
    }
  }

}
