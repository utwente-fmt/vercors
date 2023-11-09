package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{Deref, And, Block, BooleanValue, Branch, Declaration, Endpoint, EndpointUse, Expr, Loop, Node, SeqGuard, SeqProg, Statement}
import vct.col.ref.Ref
import vct.rewrite.veymont.EncodeSequentialBranchUnanimity.AddVeyMontConditionError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._

object InferSeqGuards extends RewriterBuilder {
  override def key: String = "inferSeqGuards"
  override def desc: String = "Lifts conditions in loops and conditionals into the SeqGuard AST node, stratifying the condition per endpoint."
}

case class InferSeqGuards[Pre <: Generation]() extends Rewriter[Pre] {
  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] => currentProg.having(prog) {
      rewriteDefault(prog)
    }
    case decl => rewriteDefault(decl)
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch: Branch[Pre] if currentProg.nonEmpty =>
      branch.rewrite(branch.branches.map { case (c, s) =>
          (inferSeqGuard(c), dispatch(s))
      })

    case l: Loop[Pre] if currentProg.nonEmpty =>
      l.rewrite(cond = inferSeqGuard(l.cond))

    case statement => rewriteDefault(statement)
  }

  sealed trait Target
  case class Targeted(endpoint: Endpoint[Pre]) extends Target
  case object Untargeted extends Target

  // Infer guard conditions based on the classic syntactical restrictions - endpoint dereferences determine which
  // endpoint is evaluating the expression.
  def inferSeqGuard(e: Expr[Pre]): SeqGuard[Post] = {
    val targeted = categorizeConditions(e)

    val conditions =
      if(targeted.map(_._1).forall(_ == Untargeted)) {
        // If all expressions are untargeted, spread the expressions over _all_ endpoints
        currentProg.top.endpoints.map { endpoint =>
          targeted.map { case (_, e) => (endpoint, e) }
        }
      } else {
        // Otherwise, only spread the untargeted expressions over all participating endoints
        val participants = targeted.collect { case (Targeted(endpoint), _) => endpoint }
        targeted.flatMap {
          case (Targeted(endpoint), e) => Seq((endpoint, e))
          case (Untargeted, e) => participants.map((_, e))
        }
      }

    SeqGuard[Post](conditions.map { case (endpoint, e) => (succ[Endpoint[Post]](endpoint), dispatch(e))})(e.o)
  }

  def categorizeConditions(e: Expr[Pre]): Seq[(Target, Expr[Pre])] =
    unfoldStar(e).map(e => (target(e), e))

  def target(e: Expr[Pre]): Target = {
    val endpoints: Seq[Endpoint[Pre]] = e.collect { case Deref(EndpointUse(Ref(endpoint)), _) => endpoint }
    endpoints match {
      case Seq(endpoint) => Targeted(endpoint) // expr is totally in context of one endpoint and whatever else is in scope
      case Seq() => Untargeted // Expr doesn't use any endpoints - probably fine?
      case _ => ??? // Expr uses multiple endpoints - for now we should disallow that. TODO: Throw error
    }
  }
}
