package vct.col.newrewrite

import vct.col.ast._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.result.VerificationResult.Unreachable

import scala.collection.mutable.ArrayBuffer

case object PinSilverNodes extends RewriterBuilder

case class PinSilverNodes[Pre <: Generation]() extends Rewriter[Pre] {
  def requirePredicateAccess(e: Expr[Pre]): SilverPredicateAccess[Post] = e match {
//    case Scale(perm, PredicateApply(Ref(pred), args)) =>
//      SilverPredicateAccess[Post](succ(pred), args.map(dispatch), dispatch(perm))(e.o)
//    case PredicateApply(Ref(pred), args) =>
//      SilverPredicateAccess[Post](succ(pred), args.map(dispatch), WritePerm()(e.o))(e.o)
    case other => throw Unreachable("Predicate access expected in position.")
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case assn @ Assign(target, value) => target match {
      case Local(Ref(v)) => SilverLocalAssign[Post](succ(v), dispatch(value))(stat.o)
      case SilverDeref(obj, Ref(field)) => SilverFieldAssign[Post](dispatch(obj), succ(field), dispatch(value))(assn.blame)(stat.o)
      case other => throw Unreachable("Invalid assignment target (check missing?)")
    }
    case other => rewriteDefault(other)
  }

  def collectStarall(body: Expr[Pre]): (Seq[Expr[Pre]], Expr[Pre]) = {
    val (conds, consequent) = unfoldImplies(body)
    consequent match {
      case Starall(bindings, triggers, body) =>
        bindings.foreach(dispatch)
        val (innerConds, consequent) = collectStarall(body)
        (conds ++ innerConds, consequent)
      case other =>
        (conds, other)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case CurPerm(loc) => loc match {
      case SilverDeref(obj, Ref(field)) => SilverCurFieldPerm[Post](dispatch(obj), succ(field))(e.o)
      case p =>
        val access = requirePredicateAccess(p)
        SilverCurPredPerm(access.ref, access.args)(e.o)
    }

    case SeqMember(x, Range(from, to)) =>
      implicit val o: Origin = e.o
      dispatch(from) <= dispatch(x) && dispatch(x) < dispatch(to)

    case Starall(bindings, triggers, body) =>
      implicit val o: Origin = e.o
      val newBindings = ArrayBuffer[Variable[Post]]()
      val (conds, consequent) = variableScopes.having(newBindings) {
        bindings.foreach(dispatch)
        collectStarall(body)
      }
      val newBody = foldAnd(conds.map(dispatch)) ==> dispatch(consequent)
      Starall(newBindings.toIndexedSeq, triggers.map(_.map(dispatch)), newBody)

    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TFloat() => TRational()
    case TChar() => TInt()
    case TBoundedInt(_, _) => TInt()
    case TUnion(Seq(t)) => dispatch(t)
    case other => rewriteDefault(other)
  }
}
