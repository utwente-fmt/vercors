package vct.col.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.DesugarCoalescingOperators.WrongCoalescingPredicateApply
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

object DesugarCoalescingOperators extends RewriterBuilder {
  override def key: String = "coalesce"
  override def desc: String = "Translating the ?. operator to an implication"

  case class WrongCoalescingPredicateApply(
      inv: CoalesceInstancePredicateApply[_]
  ) extends UserError {
    override def code: String = "wrongCoalesce"
    override def text: String =
      inv.o.messageInContext(
        "Applying an instance predicate with null-coalescing semantics is not supported in this position."
      )
  }
}

case class DesugarCoalescingOperators[Pre <: Generation]()
    extends Rewriter[Pre] {

  override def dispatch(inv: ApplyAnyPredicate[Pre]): ApplyAnyPredicate[Post] =
    inv match {
      case inv: CoalesceInstancePredicateApply[Pre] =>
        throw WrongCoalescingPredicateApply(inv)
      case other => other.rewriteDefault()
    }

  def makeCoalesceConditional(
      loc: Location[Pre],
      mk: Location[Post] => Expr[Post],
  ): Expr[Post] =
    loc match {
      case InLinePatternLocation(loc, pattern) =>
        makeCoalesceConditional(
          loc,
          loc => mk(InLinePatternLocation(loc, dispatch(pattern))(loc.o)),
        )
      case PredicateLocation(
            inv @ CoalesceInstancePredicateApply(obj, ref, args)
          ) =>
        implicit val o: Origin = inv.o
        Implies(
          dispatch(obj) !== Null(),
          mk(PredicateLocation(InstancePredicateApply[Post](
            dispatch(obj),
            succ(ref.decl),
            args.map(dispatch),
          ))),
        )
      case other => mk(dispatch(other))
    }

  def ignoreCoalesce(loc: Location[Pre]): Location[Post] =
    loc match {
      case PredicateLocation(
            inv @ CoalesceInstancePredicateApply(obj, ref, args)
          ) =>
        PredicateLocation[Post](
          InstancePredicateApply[Post](
            dispatch(obj),
            succ(ref.decl),
            args.map(dispatch),
          )(inv.o)
        )(loc.o)
      case other => dispatch(other)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case f: ForPerm[Pre] => f.rewrite(loc = ignoreCoalesce(f.loc))
      case c: CurPerm[Pre] => c.rewrite(loc = ignoreCoalesce(c.loc))

      case p: Perm[Pre] =>
        makeCoalesceConditional(p.loc, loc => p.rewrite(loc = loc))
      case v: Value[Pre] =>
        makeCoalesceConditional(v.loc, loc => v.rewrite(loc = loc))
      case v: AutoValue[Pre] =>
        makeCoalesceConditional(v.loc, loc => v.rewrite(loc = loc))
      case p: PVLChorPerm[Pre] =>
        makeCoalesceConditional(p.loc, loc => p.rewrite(loc = loc))
      case p: ChorPerm[Pre] =>
        makeCoalesceConditional(p.loc, loc => p.rewrite(loc = loc))

      case other => other.rewriteDefault()
    }

  def makeCoalesceConditional(
      target: FoldTarget[Pre],
      mk: FoldTarget[Post] => Statement[Post],
  ): Statement[Post] =
    target match {
      case ScaledPredicateApply(
            CoalesceInstancePredicateApply(obj, ref, args),
            perm,
          ) =>
        implicit val o: Origin = target.o
        Branch(Seq((
          dispatch(obj) !== Null(),
          mk(ScaledPredicateApply(
            InstancePredicateApply(
              dispatch(obj),
              succ(ref.decl),
              args.map(dispatch),
            ),
            dispatch(perm),
          )),
        )))
      case ValuePredicateApply(
            CoalesceInstancePredicateApply(obj, ref, args)
          ) =>
        implicit val o: Origin = target.o
        Branch(Seq((
          dispatch(obj) !== Null(),
          mk(ValuePredicateApply(InstancePredicateApply(
            dispatch(obj),
            succ(ref.decl),
            args.map(dispatch),
          ))),
        )))
      case other => mk(dispatch(other))
    }

  override def dispatch(node: Statement[Pre]): Statement[Post] =
    node match {
      case f @ Fold(target) =>
        makeCoalesceConditional(target, target => f.rewrite(res = target))
      case f @ Unfold(target) =>
        makeCoalesceConditional(target, target => f.rewrite(res = target))
      case other => other.rewriteDefault()
    }
}
