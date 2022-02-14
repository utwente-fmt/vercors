package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
import vct.col.newrewrite.util.FreshSuccessionScope
import vct.col.origin._
import vct.col.ref.{LazyRef, Ref}
import vct.col.util.AstBuildHelpers._

import scala.reflect.ClassTag

case object PropagateContextEverywhere extends RewriterBuilder {
  case class ContextEverywherePreconditionFailed(inv: InvokingNode[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inv.blame.blame(ContextEverywhereFailedInPre(error.failure, inv))
  }

  case class ContextEverywherePostconditionFailed(app: ContractApplicable[_]) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      app.blame.blame(ContextEverywhereFailedInPost(error.failure, app))
  }
}

case class PropagateContextEverywhere[Pre <: Generation]() extends Rewriter[Pre] {
  import PropagateContextEverywhere._

  val invariants: ScopedStack[Seq[Expr[Pre]]] = ScopedStack()
  invariants.push(Nil)

  def withInvariant[T](inv: Expr[Pre])(f: => T): T = {
    val old = invariants.top
    invariants.pop()
    invariants.push(old ++ unfoldStar(inv))
    val result = f
    invariants.pop()
    invariants.push(old)
    result
  }

  def freshInvariants()(implicit o: Origin): Expr[Post] =
    foldStar(invariants.top.map(inv => FreshSuccessionScope(this).dispatch(inv)))

  def booleanInvariants: Seq[Expr[Pre]] = invariants.top.filter(inv => TBool().superTypeOf(inv.t))

  def freshBooleanInvariants()(implicit o: Origin): Expr[Post] =
    foldAnd(booleanInvariants.map(inv => FreshSuccessionScope(this).dispatch(inv)))

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: ContractApplicable[Pre] =>
      (withInvariant(app.contract.contextEverywhere) {
        app match {
          case func: AbstractFunction[Pre] =>
            func.rewrite(blame = ContractedBlameSplit.left(ContextEverywherePostconditionFailed(app), func.blame))
          case method: AbstractMethod[Pre] =>
            method.rewrite(blame = ImplBlameSplit.left(ContextEverywherePostconditionFailed(app), method.blame))
        }
      }).succeedDefault(app)
    case other => rewriteDefault(other)
  }

  override def dispatch(contract: ApplicableContract[Pre]): ApplicableContract[Post] = {
    implicit val o: Origin = contract.o
    contract.rewrite(
      contextEverywhere = tt,
      requires = SplitAccountedPredicate(UnitAccountedPredicate(freshInvariants()), dispatch(contract.requires)),
      ensures = SplitAccountedPredicate(UnitAccountedPredicate(freshInvariants()), dispatch(contract.ensures)),
    )
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case inv: Invocation[Pre] =>
      inv.rewrite(blame = InvBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case inv: InvocationStatement[Pre] =>
      inv.rewrite(blame = InvBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case bar: ParBarrier[Pre] =>
      implicit val o: Origin = bar.o
      bar.rewrite(
        requires = freshBooleanInvariants() &* dispatch(bar.requires),
        ensures = freshBooleanInvariants() &* dispatch(bar.ensures),
      )
    case loop: Loop[Pre] =>
      implicit val o: Origin = loop.o
      loop.contract match {
        case inv @ LoopInvariant(invariant) =>
          loop.rewrite(contract = LoopInvariant(freshInvariants() &* dispatch(invariant))(inv.blame))
        case c @ IterationContract(requires, ensures, context_everywhere) =>
          val (contract, body) = invariants.having(booleanInvariants) {
            (c.rewrite(context_everywhere = freshBooleanInvariants() && dispatch(context_everywhere)), dispatch(loop.body))
          }
          // update and init still have the outer invariants.
          loop.rewrite(contract = contract, body = body)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(parRegion: ParRegion[Pre]): ParRegion[Post] = parRegion match {
    case block: ParBlock[Pre] =>
      implicit val o: Origin = parRegion.o
      invariants.having(booleanInvariants ++ unfoldStar(block.context_everywhere)) {
        block.rewrite(
          context_everywhere = freshBooleanInvariants()
        )
      }
    case other => rewriteDefault(other)
  }
}
