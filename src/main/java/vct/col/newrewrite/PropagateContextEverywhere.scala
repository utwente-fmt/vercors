package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
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

  val invariants: ScopedStack[Expr[Pre]] = ScopedStack()

  case class ClosedCopier() extends Rewriter[Pre] {
    override def succ[DPost <: Declaration[Post]](decl: Declaration[Pre])(implicit tag: ClassTag[DPost]): Ref[Post, DPost] =
      new LazyRef[Post, DPost](
        successionMap.get(decl).getOrElse(PropagateContextEverywhere.this.successionMap(decl))
      )
  }

  def freshInvariants()(implicit o: Origin): Expr[Post] = foldStar(invariants.toSeq.map(inv => ClosedCopier().dispatch(inv)))

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: ContractApplicable[Pre] =>
      (invariants.having(app.contract.contextEverywhere) {
        app match {
          case func: AbstractFunction[Pre] =>
            func.rewrite(blame = ContractedBlameSplit.left(ContextEverywherePostconditionFailed(app), func.blame))
          case method: AbstractMethod[Pre] =>
            method.rewrite(blame = ImplBlameSplit.left(ContextEverywherePostconditionFailed(app), method.blame))
        }
      }).succeedDefault(this, app)
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
    case other => rewriteDefault(other)
  }

  override def dispatch(node: LoopContract[Pre]): LoopContract[Post] = node match {
    case LoopInvariant(invariant) =>
      implicit val o: Origin = node.o
      LoopInvariant(freshInvariants() &* dispatch(invariant))
    case other => rewriteDefault(other)
  }
}
