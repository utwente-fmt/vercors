package vct.col.ast.statement.veymont

import vct.col.ast.{Access, Communicate, Endpoint, EndpointGuard, EndpointName, SeqAssign, SeqBranch, SeqLoop, UnpointedGuard}
import vct.col.ref.Ref

import scala.collection.immutable.ListSet

trait SeqLoopImpl[G] { this: SeqLoop[G] =>
  def hasUnpointed: Boolean = guards.exists { case _: UnpointedGuard[G] => true; case _ => false }
  def explicitParticipants: Seq[Endpoint[G]] = guards.collect { case EndpointGuard(Ref(endpoint), condition) => endpoint }

  def participants: Set[Endpoint[G]] =
    ListSet.from(subnodes.collect {
      case Communicate(Access(EndpointName(Ref(receiver)), _), Access(EndpointName(Ref(sender)), _)) => Seq(receiver, sender)
      case SeqAssign(Ref(receiver), _, _) => Seq(receiver)
      case branch: SeqBranch[G] => branch.explicitParticipants
    }.flatten)
}
