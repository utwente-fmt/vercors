package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.SplitSeqGuards.MultipleEndpoints

import scala.collection.immutable.ListSet

object EncodeUnpointedGuard extends RewriterBuilder {
  override def key: String = "encodeUnpointedGuard"
  override def desc: String = "Encodes unpointed guard by duplicating the condition to all guards currently participating"
}

case class EncodeUnpointedGuard[Pre <: Generation]() extends Rewriter[Pre] {
  val currentParticipants: ScopedStack[ListSet[Endpoint[Pre]]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] => currentParticipants.having(ListSet.from(prog.endpoints)) {
      rewriteDefault(prog)
    }

    case decl => rewriteDefault(decl)
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch: SeqBranch[Pre] =>
      val newParticipants = if (branch.hasUnpointed) {
        currentParticipants.top
      } else {
        ListSet.from(branch.participants)
      }
      currentParticipants.having(newParticipants) {
        branch.rewrite(guards = branch.guards.flatMap(rewriteGuard))
      }

    case loop: SeqLoop[Pre] =>
      val newParticipants = if (loop.hasUnpointed) {
        currentParticipants.top
      } else {
        ListSet.from(loop.participants)
      }
      currentParticipants.having(newParticipants) {
        loop.rewrite(guards = loop.guards.flatMap(rewriteGuard))
      }

    case statement => rewriteDefault(statement)
  }

  def rewriteGuard(guard: SeqGuard[Pre]): Seq[SeqGuard[Post]] = guard match {
    case guard: EndpointGuard[Pre] => Seq(guard.rewriteDefault())
    case UnpointedGuard(expr) => currentParticipants.top.map { endpoint =>
      EndpointGuard[Post](succ(endpoint), dispatch(expr))(guard.o)
    }.toSeq
  }
}
