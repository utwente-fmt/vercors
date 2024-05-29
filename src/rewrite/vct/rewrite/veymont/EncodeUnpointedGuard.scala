package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.SplitChorGuards.MultipleEndpoints

import scala.collection.immutable.ListSet

object EncodeUnpointedGuard extends RewriterBuilder {
  override def key: String = "encodeUnpointedGuard"
  override def desc: String =
    "Encodes unpointed guard by duplicating the condition to all guards currently participating"
}

case class EncodeUnpointedGuard[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] {
  val currentParticipants: ScopedStack[ListSet[Endpoint[Pre]]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) {
          currentParticipants.having(ListSet.from(chor.endpoints)) {
            rewriteDefault(chor)
          }
        }

      case decl => rewriteDefault(decl)
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case InChor(_, c @ ChorStatement(branch: Branch[Pre])) =>
        // All Chor branches must have two branches, of which the second one has a tt condition
        val (guard, yes, noOpt) =
          branch.branches match {
            case Seq((guard, yes)) => (guard, yes, None)
            case Seq((guard, yes), (BooleanValue(true), no)) =>
              (guard, yes, Some(no))
            case _ => ???
          }
        val newParticipants =
          if (c.branch.hasUnpointed) { currentParticipants.top }
          else { ListSet.from(c.participants) }
        currentParticipants.having(newParticipants) {
          c.rewrite(inner =
            branch.rewrite(branches =
              (rewriteGuards(guard), super.dispatch(yes)) +:
                noOpt.map(no => Seq((tt[Post], super.dispatch(no))))
                  .getOrElse(Seq())
            )
          )
        }

      case InChor(_, c @ ChorStatement(loop: Loop[Pre])) =>
        val newParticipants =
          if (c.loop.hasUnpointed) { currentParticipants.top }
          else { ListSet.from(c.participants) }
        currentParticipants.having(newParticipants) {
          c.rewrite(inner = loop.rewrite(cond = rewriteGuards(loop.cond)))
        }

      case statement => statement.rewriteDefault()
    }

  def rewriteGuards(guard: Expr[Pre]): Expr[Post] = {
    implicit val o = guard.o
    foldAnd(unfoldStar(guard).flatMap {
      case expr: EndpointExpr[Pre] => Seq(expr.rewriteDefault())
      case expr =>
        currentParticipants.top.map { endpoint =>
          EndpointExpr[Post](succ(endpoint), dispatch(expr))(guard.o)
        }.toSeq
    })
  }
}
