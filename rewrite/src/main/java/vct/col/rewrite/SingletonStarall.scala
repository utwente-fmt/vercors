package vct.col.rewrite
import vct.col.ast._
import vct.col.origin.{Blame, Origin, ReceiverNotInjective}
import vct.col.rewrite.SingletonStarall.UnknownStarallFormat
import vct.col.util.AstBuildHelpers.{ExprBuildHelpers, foldAnd, foldStar}
import vct.result.VerificationError.SystemError

case object SingletonStarall extends RewriterBuilder {
  override def key: String = "singletonStarall"
  override def desc: String = "Convert multi-resource staralls to single-resource staralls"

  case class UnknownStarallFormat(e: Expr[_]) extends SystemError {
    override def text: String = e.o.messageInContext("This expression should probably not occur in a ∀*")
  }
}

/**
 * Starall's that have multiple resources are expanded into several starall's with one resource. This is mostly already
 * done by the simplifier, but some passes after the simplifier may introduce them.
 */
case class SingletonStarall[Pre <: Generation]() extends Rewriter[Pre] {
  def expand(bindings: Seq[Variable[Pre]],
             triggers: Seq[Seq[Expr[Pre]]],
             blame: Blame[ReceiverNotInjective],
             conds: Seq[Expr[Pre]],
             body: Expr[Pre])(implicit o: Origin): Seq[Expr[Post]] = body match {
    case Perm(_, _) | Value(_) | PredicateApply(_, _, _) => variables.scope { Seq(
      Starall(variables.dispatch(bindings), triggers.map(_.map(dispatch)), foldAnd(conds.map(dispatch)) ==> dispatch(body))(blame)
    ) }
    case body if body.t == TBool[Pre]() => variables.scope { Seq(
        Forall(variables.dispatch(bindings), triggers.map(_.map(dispatch)), foldAnd(conds.map(dispatch)) ==> dispatch(body))
    ) }

    case Star(left, right) => expand(bindings, triggers, blame, conds, left) ++ expand(bindings, triggers, blame, conds, right)
    case Implies(cond, res) =>
      expand(bindings, triggers, blame, conds :+ cond, res)
    case Select(cond, left, right) =>
      expand(bindings, triggers, blame, conds :+ cond, left) ++ expand(bindings, triggers, blame, conds :+ !cond, right)
    case other => throw UnknownStarallFormat(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case starall @ Starall(bindings, triggers, body) =>
      foldStar(expand(bindings, triggers, starall.blame, Nil, body)(e.o))(e.o)
    case other => rewriteDefault(other)
  }
}
