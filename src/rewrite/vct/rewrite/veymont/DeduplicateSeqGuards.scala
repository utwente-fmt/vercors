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

import scala.collection.mutable

object DeduplicateSeqGuards extends RewriterBuilder {
  override def key: String = "deduplicateSeqGuards"
  override def desc: String = "Deduplicates SeqGuard nodes with syntactically identical endpoints"
}

case class DeduplicateSeqGuards[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch: SeqBranch[Pre] =>
      val guards: Seq[EndpointGuard[Pre]] = branch.guards.map {
        case guard: EndpointGuard[Pre] => guard
        case guard: UnpointedGuard[Pre] => ??? // Excluded by RemoveUnpointedGuard
      }
      branch.rewrite(guards = dedup(guards))

    case loop: SeqLoop[Pre] =>
      val guards: Seq[EndpointGuard[Pre]] = loop.guards.map {
        case guard: EndpointGuard[Pre] => guard
        case guard: UnpointedGuard[Pre] => ??? // Excluded by RemoveUnpointedGuard
      }
      loop.rewrite(guards = dedup(guards))

    case _ => rewriteDefault(statement)
  }

  def dedup(guards: Seq[EndpointGuard[Pre]]): Seq[EndpointGuard[Post]] = {
    val m: mutable.LinkedHashMap[Endpoint[Pre], Seq[Expr[Pre]]] = mutable.LinkedHashMap()
    guards.foreach { guard =>
      m.updateWith(guard.endpoint.decl)(exprs => Some(exprs.getOrElse(Nil) :+ guard.condition))
    }
    m.iterator.map { case (endpoint, exprs) =>
      EndpointGuard[Post](succ(endpoint), foldAnd(exprs.map(dispatch))(DiagnosticOrigin))(exprs.head.o)
    }.toSeq
  }
}
