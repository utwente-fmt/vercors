package vct.col.newrewrite

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.ast._
import vct.col.origin.{Blame, Origin, ReceiverNotInjective}
import vct.col.util.AstBuildHelpers._

import scala.annotation.tailrec

case object RejoinQuantifiers extends RewriterBuilder {
  override def key: String = "rejoinQuantifiers"
  override def desc: String = "Collapse nested quantifiers into one with multiple bindings, to admit more triggers again."
}

case class RejoinQuantifiers[Pre <: Generation]() extends Rewriter[Pre] {
  case class RollupState(body: Expr[Pre], triggers: Seq[Seq[Expr[Pre]]] = Nil, bindings: Seq[Variable[Pre]] = Nil, conditions: Seq[Expr[Pre]] = Nil) {
    @tailrec
    final def fixpoint(f: PartialFunction[RollupState, RollupState]): RollupState =
      f.lift(this) match {
        case Some(value) => value.fixpoint(f)
        case None => this
      }

    def makeForall(implicit o: Origin): Forall[Post] =
      variables.scope {
        Forall(variables.dispatch(bindings), triggers.map(_.map(dispatch)), foldAnd(conditions.map(dispatch)) ==> dispatch(body))
      }

    def makeStarall(blame: Blame[ReceiverNotInjective])(implicit o: Origin): Starall[Post] =
      variables.scope {
        Starall(variables.dispatch(bindings), triggers.map(_.map(dispatch)), foldAnd(conditions.map(dispatch)) ==> dispatch(body))(blame)
      }

    def makeExists(implicit o: Origin): Exists[Post] =
      variables.scope {
        Exists(variables.dispatch(bindings), triggers.map(_.map(dispatch)), foldAnd(conditions.map(dispatch) :+ dispatch(body)))
      }
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case f: Forall[Pre] => RollupState(f).fixpoint {
      case RollupState(Forall(moreBindings, triggers, body), _, bindings, conditions) =>
        RollupState(body, triggers, bindings ++ moreBindings, conditions)

      case RollupState(Implies(cond, body), triggers, bindings, conditions) =>
        RollupState(body, triggers, bindings, conditions :+ cond)
    }.makeForall(f.o)

    case s: Starall[Pre] => RollupState(s).fixpoint {
      case RollupState(Starall(moreBindings, triggers, body), _, bindings, conditions) =>
        RollupState(body, triggers, bindings ++ moreBindings, conditions)
      case RollupState(Forall(moreBindings, triggers, body), _, bindings, conditions) =>
        RollupState(body, triggers, bindings ++ moreBindings, conditions)

      case RollupState(Implies(cond, body), triggers, bindings, conditions) =>
        RollupState(body, triggers, bindings, conditions :+ cond)
    }.makeStarall(s.blame)(s.o)

    case e: Exists[Pre] => RollupState(e).fixpoint {
      case RollupState(Exists(moreBindings, triggers, body), _, bindings, conditions) =>
        RollupState(body, triggers, bindings ++ moreBindings, conditions)

      case RollupState(And(left, right), triggers, bindings, conditions) =>
        RollupState(right, triggers, bindings, conditions :+ left)
    }.makeExists(e.o)

    case other => rewriteDefault(other)
  }

}
