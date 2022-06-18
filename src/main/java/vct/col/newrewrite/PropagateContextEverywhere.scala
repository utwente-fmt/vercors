package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.newrewrite.error.ExtraNode
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

case object PropagateContextEverywhere extends RewriterBuilder {
  override def key: String = "propagateContext"
  override def desc: String = "Propagate context_everywhere declarations into loop invariants and parallel block contracts."

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
    foldStar(invariants.top.map(inv => freshSuccessionScope { dispatch(inv) }))

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: ContractApplicable[Pre] =>
      (withInvariant(app.contract.contextEverywhere) {
        app match {
          case func: AbstractFunction[Pre] =>
            func.rewrite(blame = PostBlameSplit.left(ContextEverywherePostconditionFailed(app), func.blame))
          case method: AbstractMethod[Pre] =>
            method.rewrite(blame = PostBlameSplit.left(ContextEverywherePostconditionFailed(app), method.blame))
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
    case inv: ProcedureInvocation[Pre] =>
      inv.rewrite(blame = PreBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case inv: MethodInvocation[Pre] =>
      inv.rewrite(blame = PreBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case inv: FunctionInvocation[Pre] =>
      inv.rewrite(blame = PreBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case inv: InstanceFunctionInvocation[Pre] =>
      inv.rewrite(blame = PreBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case inv: InvokeProcedure[Pre] =>
      inv.rewrite(blame = PreBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case inv: InvokeMethod[Pre] =>
      inv.rewrite(blame = PreBlameSplit.left(ContextEverywherePreconditionFailed(inv), inv.blame))
    case bar: ParBarrier[Pre] =>
      implicit val o: Origin = bar.o
      bar.rewrite(
        requires = freshInvariants() &* dispatch(bar.requires),
        ensures = freshInvariants() &* dispatch(bar.ensures),
      )
    case loop: Loop[Pre] =>
      implicit val o: Origin = loop.o
      loop.contract match {
        case inv @ LoopInvariant(invariant) =>
          loop.rewrite(contract = LoopInvariant(freshInvariants() &* dispatch(invariant))(inv.blame))
        case _: IterationContract[Pre] => throw ExtraNode
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(parRegion: ParRegion[Pre]): ParRegion[Post] = parRegion match {
    case block: ParBlock[Pre] =>
      implicit val o: Origin = parRegion.o

      val scaledInvariants = invariants.top.map(inv => ScaleByParBlock[Pre](block.decl.ref, inv))

      invariants.having(scaledInvariants ++ unfoldStar(block.context_everywhere)) {
        block.rewrite(
          context_everywhere = freshInvariants()
        )
      }
    case ParParallel(regions) =>
      implicit val o: Origin = parRegion.o
      val scaledInvariants = invariants.top.map(inv => Scale[Pre](const[Pre](1) /:/ const(regions.size), inv)(
        PanicBlame("If used, the number of regions is strictly positive")))

      invariants.having(scaledInvariants) {
        rewriteDefault(parRegion)
      }
    case ParSequential(_) => rewriteDefault(parRegion)
  }
}
