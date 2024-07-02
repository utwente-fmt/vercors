package vct.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.result.VerificationError.UserError
import vct.rewrite.DisambiguatePredicateExpression.{
  AbstractFoldTarget,
  WrongFoldTarget,
}

object DisambiguatePredicateExpression extends RewriterBuilder {
  override def key: String = "disambiguatePredicate"
  override def desc: String =
    "Translate the positions where a predicate application occurs as a plain expression"

  case class WrongFoldTarget(e: Expr[_]) extends UserError {
    override def code: String = "wrongFoldExpr"
    override def text: String =
      e.o.messageInContext(
        "The subject of a fold or unfold must be a predicate application, which may be wrapped in a scale, Perm, or Value."
      )
  }

  case class AbstractFoldTarget(e: Expr[_]) extends UserError {
    override def code: String = "abstractFoldExpr"
    override def text: String =
      e.o.messageInContext(
        "The predicate that is (un)folded may not be abstract."
      )
  }
}

case class DisambiguatePredicateExpression[Pre <: Generation]()
    extends Rewriter[Pre] {

  def predInvForTarget(e: Expr[Pre]): ApplyAnyPredicate[Post] =
    e match {
      case PredicateApplyExpr(inv: ApplyAnyPredicate[Pre])
          if inv.ref.decl.body.isEmpty =>
        throw AbstractFoldTarget(e)

      case PredicateApplyExpr(inv: ApplyAnyPredicate[Pre]) => dispatch(inv)
      case other => throw WrongFoldTarget(other)
    }

  def foldTarget(e: Expr[Pre]): FoldTarget[Post] =
    e match {
      case Scale(scale, e) =>
        ScaledPredicateApply(predInvForTarget(e), dispatch(scale))(e.o)
      case Perm(PredicateLocation(inv), perm) =>
        ScaledPredicateApply(dispatch(inv), dispatch(perm))(e.o)
      case Value(PredicateLocation(inv)) =>
        ValuePredicateApply(dispatch(inv))(e.o)
      case other =>
        ScaledPredicateApply(predInvForTarget(other), WritePerm()(e.o))(e.o)
    }

  override def dispatch(node: FoldTarget[Pre]): FoldTarget[Rewritten[Pre]] =
    node match {
      case AmbiguousFoldTarget(e) => foldTarget(e)
      case other => other.rewriteDefault()
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case PredicateApplyExpr(inv: ApplyAnyPredicate[Pre]) =>
        implicit val o: Origin = inv.o
        Perm(PredicateLocation(dispatch(inv)), WritePerm())

      case e => e.rewriteDefault()
    }
}
