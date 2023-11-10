package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.SplitSeqGuards.MultipleEndpoints

object SplitSeqGuards extends RewriterBuilder {
  override def key: String = "inferSeqGuards"
  override def desc: String = "Lifts conditions in loops and conditionals into the SeqGuard AST node, stratifying the condition per endpoint."

  case class MultipleEndpoints(e: Expr[_]) extends UserError {
    override def code: String = "multipleEndpoints"
    override def text: String = e.o.messageInContext("This expression references multiple endpoints, but that is not yet supported.")
  }
}

case class SplitSeqGuards[Pre <: Generation]() extends Rewriter[Pre] {
  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] =>
      currentProg.having(prog) {
        rewriteDefault(prog)
      }
    case decl => rewriteDefault(decl)
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch: Branch[Pre] if currentProg.nonEmpty =>
      assert(branch.branches.nonEmpty)
      unfoldBranch(branch.branches)(branch.o)

    case l: Loop[Pre] if currentProg.nonEmpty =>
      ???

    case statement => rewriteDefault(statement)
  }

  def unfoldBranch(branches: Seq[(Expr[Pre], Statement[Pre])])(implicit o: Origin): SeqBranch[Post] = branches match {
    case Seq((e, s)) => SeqBranch(inferSeqGuard(e), dispatch(s), None)
    case (e, s) +: (otherYes +: branches) =>
      SeqBranch(inferSeqGuard(e), dispatch(s), Some(unfoldBranch(otherYes +: branches)))
  }

  // Infer guard conditions based on the classic syntactical restrictions - endpoint dereferences determine which
  // endpoint is evaluating the expression.
  def inferSeqGuard(e: Expr[Pre]): Seq[SeqGuard[Post]] =
    unfoldStar(e).map(target)

  // This method makes an experssion targeted, in the sense that: if it is syntactically obvious the expression is to be
  // executed in the context of a certain endpoint, it is marked as a SeqGuard belonging to that endpoint
  def target(e: Expr[Pre]): SeqGuard[Post] = {
    val endpoints: Seq[Endpoint[Pre]] = e.collect { case Deref(EndpointUse(Ref(endpoint)), _) => endpoint }
    endpoints match {
      case Seq(endpoint) =>
        // expr is totally in context of one endpoint and whatever else is in scope
        EndpointGuard[Post](succ(endpoint), dispatch(e))(e.o)
      case Seq() => UnpointedGuard(dispatch(e))(e.o) // Expr doesn't use any endpoints - probably fine?
      case _ => throw MultipleEndpoints(e) // Expr uses multiple endpoints - for now we should disallow that.
    }
  }
}
