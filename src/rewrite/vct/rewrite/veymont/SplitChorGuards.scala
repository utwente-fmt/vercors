package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.check.SeqProgParticipant
import vct.col.origin.{Blame, Origin, SeqBranchFailure}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.SplitChorGuards.{MultipleEndpoints, SeqProgParticipantErrors}

import scala.collection.immutable.ListSet
import scala.collection.mutable

object SplitChorGuards extends RewriterBuilder {
  override def key: String = "splitChorGuards"
  override def desc: String = "Lifts conditions in loops and conditionals into the ChorGuard AST node, stratifying the condition per endpoint."

  case class MultipleEndpoints(e: Expr[_]) extends UserError {
    override def code: String = "multipleEndpoints"
    override def text: String = e.o.messageInContext("This expression references multiple endpoints, but that is not yet supported.")
  }

  case class SeqProgParticipantErrors(es: Seq[SeqProgParticipant]) extends UserError {
    override def code: String = "seqProgParticipantErrors"
    override def text: String = es.map {
      case err: SeqProgParticipant => err.message { n => n.o }
    }.mkString("\n")
  }
}

case class SplitChorGuards[Pre <: Generation]() extends Rewriter[Pre] {
  val currentProg: ScopedStack[Choreography[Pre]] = ScopedStack()
  val currentParticipants: ScopedStack[ListSet[Pre]] = ScopedStack()

  override def dispatch(prog: Program[Pre]): Program[Post] = {
    val newProg = prog.rewrite()
    val errors = newProg.check
    val seqBranchErrors = errors.collect {
      case err: SeqProgParticipant => err
    }
    if (errors.nonEmpty && errors.length == seqBranchErrors.length) {
      throw SeqProgParticipantErrors(seqBranchErrors)
    }
    newProg
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: Choreography[Pre] =>
      currentProg.having(prog) {
        rewriteDefault(prog)
      }
    case decl => rewriteDefault(decl)
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch: UnresolvedChorBranch[Pre] =>
      assert(branch.branches.nonEmpty)
      unfoldBranch(branch.branches)(branch.blame, branch.o)

    case loop: UnresolvedChorLoop[Pre] if currentProg.nonEmpty =>
      ChorLoop(
        inferSeqGuard(loop.cond),
        dispatch(loop.contract),
        dispatch(loop.body)
      )(loop.blame)(loop.o)

    case statement => rewriteDefault(statement)
  }

  def unfoldBranch(branches: Seq[(Expr[Pre], Statement[Pre])])(implicit blame: Blame[SeqBranchFailure], o: Origin): ChorBranch[Post] = branches match {
    case Seq((e, s)) => ChorBranch(inferSeqGuard(e), dispatch(s), None)(blame)
    case (e, s) +: (otherYes +: branches) =>
      ChorBranch(inferSeqGuard(e), dispatch(s), Some(unfoldBranch(otherYes +: branches)))(blame)
    case _ => ???
  }

  // Infer guard conditions based on the classic syntactical restrictions - endpoint dereferences determine which
  // endpoint is evaluating the expression.
  def inferSeqGuard(e: Expr[Pre]): Seq[ChorGuard[Post]] = {
    val exprs = {
      // Ensure the "true" expression is kept
      val es = unfoldStar(e)
      if (es.isEmpty) Seq(e) else es
    }
    val pointed = exprs.map(point)
    pointed.map {
      case (Some(endpoint), expr) => EndpointGuard[Post](succ(endpoint), dispatch(expr))(expr.o)
      case (None, expr) => UnpointedGuard(dispatch(expr))(expr.o)
    }
  }

  // "Points" an expression in the direction of an endpoint if possible
  def point(e: Expr[Pre]): (Option[Endpoint[Pre]], Expr[Pre]) = {
    InferEndpointContexts.getEndpoints(e) match {
      case Seq(endpoint) =>
        // expr is totally in context of one endpoint and whatever else is in scope
        (Some(endpoint), e)
      case Seq() => (None, e)
      case _ => throw MultipleEndpoints(e) // Expr uses multiple endpoints - for now we should disallow that.
    }
  }
}
