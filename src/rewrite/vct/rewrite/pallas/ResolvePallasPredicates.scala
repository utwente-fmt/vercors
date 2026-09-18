package vct.rewrite.pallas

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{LabelContext, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.{StatementToExpression, SuccessionMap}
import vct.result.VerificationError.UserError

case object ResolvePallasPredicates extends RewriterBuilder {
  override def key: String = "resolvePallasPredicates"
  override def desc: String =
    "Turn Pallas predicates that still contain statements into actual predicates."

}

case class ResolvePallasPredicates[Pre <: Generation]() extends Rewriter[Pre] {
  import ResolvePallasPredicates._

  private val predMap
      : SuccessionMap[LLVMPredicateDefinition[Pre], Predicate[Post]] =
    SuccessionMap()

  case class PredicateConversionError(
      pred: LLVMPredicateDefinition[_],
      expl: String,
  ) extends UserError {
    override def code: String = "predConvError"
    override def text: String =
      pred.o
        .messageInContext(s"The Pallas predicate cannot be converted ($expl).")
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case pred: LLVMPredicateDefinition[Pre] =>
        val newPred = labelDecls.scope {
          globalDeclarations.declare(
            new Predicate[Post](
              args = variables.dispatch(pred.args),
              body = pred.body.map(
                StatementToExpression(
                  this,
                  (s: String) => PredicateConversionError(pred, s),
                ).toExpression(_, None).getOrElse(
                  throw PredicateConversionError(
                    pred,
                    "the body cannot be restructured into a pure expression",
                  )
                )
              ),
              threadLocal = pred.threadLocal,
              inline = pred.inline,
              pallasPredicate = true,
            )(pred.o)
          )
        }
        predMap.update(pred, newPred)
      // globalDeclarations.succeed(pred, newPred)
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(
      a: ApplyAnyPredicate[Pre]
  ): ApplyAnyPredicate[Rewritten[Pre]] = {
    a match {
      case pApply: LLVMPredicateApply[Pre] =>
        PredicateApply[Post](
          ref = predMap.ref(pApply.ref.decl),
          args = pApply.args.map(dispatch),
        )(pApply.o)
      case other => other.rewriteDefault()
    }
  }
}
