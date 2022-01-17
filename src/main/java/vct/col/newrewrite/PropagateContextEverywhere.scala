package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.ref.{LazyRef, Ref}
import vct.col.util.AstBuildHelpers._

import scala.reflect.ClassTag

case object PropagateContextEverywhere extends RewriterBuilder

case class PropagateContextEverywhere[Pre <: Generation]() extends Rewriter[Pre] {
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
      invariants.having(app.contract.contextEverywhere) {
        rewriteDefault(app)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(contract: ApplicableContract[Pre]): ApplicableContract[Post] = {
    implicit val o: Origin = contract.o
    contract.rewrite(
      contextEverywhere = tt,
      requires = freshInvariants() &* dispatch(contract.requires),
      ensures = freshInvariants() &* dispatch(contract.ensures),
    )
  }

  override def dispatch(node: LoopContract[Pre]): LoopContract[Post] = node match {
    case LoopInvariant(invariant) =>
      implicit val o: Origin = node.o
      LoopInvariant(freshInvariants() &* dispatch(invariant))
    case other => rewriteDefault(other)
  }
}
