package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.SplitSeqGuards.MultipleEndpoints

import scala.collection.immutable.ListSet

object RemoveUnpointedGuard extends RewriterBuilder {
  override def key: String = "removeUnpointedGuard"
  override def desc: String = "Removes unpointed guard by duplicating the condition to all guards currently participating"
}

case class RemoveUnpointedGuard[Pre <: Generation]() extends Rewriter[Pre] {
  val currentParticipants: ScopedStack[ListSet[Endpoint[Pre]]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] => currentParticipants.having(ListSet.from(prog.endpoints)) {
      rewriteDefault(prog)
    }

    case decl => rewriteDefault(decl)
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch@SeqBranch(guards, yes, no) =>
      val newParticipants = if (branch.hasUnpointed) {
        currentParticipants.top
      } else {
        ListSet.from(branch.participants)
      }
      currentParticipants.having(newParticipants) {
        SeqBranch(
          branch.guards.flatMap(rewriteGuard),
          dispatch(yes),
          no.map(dispatch)
        )(branch.o)
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
