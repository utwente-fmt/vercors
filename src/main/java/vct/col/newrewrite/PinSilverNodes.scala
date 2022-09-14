package vct.col.newrewrite

import vct.col.ast._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.result.VerificationError.Unreachable

import scala.collection.mutable.ArrayBuffer

case object PinSilverNodes extends RewriterBuilder {
  override def key: String = "pinSilverNodes"
  override def desc: String = "Disambiguate nodes into specific Silver nodes."
}


case class PinSilverNodes[Pre <: Generation]() extends Rewriter[Pre] {
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
    case SeqMember(x, Range(from, to)) =>
      implicit val o: Origin = e.o
      dispatch(from) <= dispatch(x) && dispatch(x) < dispatch(to)

    case starall @ Starall(bindings, triggers, body) =>
      implicit val o: Origin = e.o
      val (newBindings, (conds, consequent)) = variables.collect {
        bindings.foreach(dispatch)
        collectStarall(body)
      }
      val newBody = foldAnd(conds.map(dispatch)) ==> dispatch(consequent)
      Starall(newBindings, triggers.map(_.map(dispatch)), newBody)(starall.blame)

    case Size(xs) =>
      if(xs.t.asSet.nonEmpty) SilverSetSize(dispatch(xs))(e.o)
      else if(xs.t.asBag.nonEmpty) SilverBagSize(dispatch(xs))(e.o)
      else if(xs.t.asMap.nonEmpty) SilverMapSize(dispatch(xs))(e.o)
      else SilverSeqSize(dispatch(xs))(e.o)

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
