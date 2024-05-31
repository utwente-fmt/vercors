package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.check.SeqProgParticipant
import vct.col.origin.{Blame, Origin, SeqBranchFailure}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.StratifyExpressions.{
  MultipleEndpoints,
  SeqProgParticipantErrors,
}

import scala.collection.immutable.ListSet
import scala.collection.mutable

object StratifyExpressions extends RewriterBuilder {
  override def key: String = "stratifyExpressions"
  override def desc: String =
    "Stratifies expressions by putting all contracts, branch conditions and loop conditions within a choreography's run declaration into endpoint exprs, inferring endpoint contexts where required."

  case class MultipleEndpoints(e: Expr[_]) extends UserError {
    override def code: String = "multipleEndpoints"
    override def text: String =
      e.o.messageInContext(
        "This expression references multiple endpoints, but that is not yet supported."
      )
  }

  case class SeqProgParticipantErrors(es: Seq[SeqProgParticipant])
      extends UserError {
    override def code: String = "seqProgParticipantErrors"
    override def text: String =
      es.map { case err: SeqProgParticipant => err.message { n => n.o } }
        .mkString("\n")
  }
}

case class StratifyExpressions[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {

  override def dispatch(prog: Program[Pre]): Program[Post] = {
    val newProg = prog.rewrite()
    val errors = newProg.check
    val seqBranchErrors = errors.collect { case err: SeqProgParticipant => err }
    if (errors.nonEmpty && errors.length == seqBranchErrors.length) {
      throw SeqProgParticipantErrors(seqBranchErrors)
    }
    newProg
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case prog: Choreography[Pre] =>
        currentChoreography.having(prog) { prog.rewriteDefault().succeed(prog) }
      case decl => super.dispatch(decl)
    }

  override def dispatch(
      contract: ApplicableContract[Pre]
  ): ApplicableContract[Post] =
    contract match {
      case InChor(_, contract) =>
        contract.rewrite(
          requires = (stratifyExpr(_)).accounted(contract.requires),
          ensures = (stratifyExpr(_)).accounted(contract.ensures),
          contextEverywhere = stratifyExpr(contract.contextEverywhere),
        )
      case _ => contract.rewriteDefault()
    }

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    contract match {
      case InChor(_, inv: LoopInvariant[Pre]) =>
        inv.rewrite(invariant = stratifyExpr(inv.invariant))
      case InChor(_, contract: IterationContract[Pre]) =>
        contract.rewrite(
          requires = stratifyExpr(contract.requires),
          ensures = stratifyExpr(contract.ensures),
        )
      case _ => contract.rewriteDefault()
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case InChor(_, l: Loop[Pre]) if currentChoreography.nonEmpty =>
        loop(
          cond = stratifyExpr(l.cond),
          contract = dispatch(l.contract),
          body = dispatch(l.body),
        ) /*(loop.blame)*/ (l.o)

      case InChor(_, branch: Branch[Pre]) =>
        assert(branch.branches.nonEmpty)
        logger.warn("TODO: Branch blame")
        unfoldBranch(branch.branches)(null, branch.o)

      case statement => statement.rewriteDefault()
    }

  // TODO (RR): For branch, make sure blame is put on ChorStatement wrapper of Branch. Probably too for loop

  def unfoldBranch(
      branches: Seq[(Expr[Pre], Statement[Pre])]
  )(implicit blame: Blame[SeqBranchFailure], o: Origin): Branch[Post] =
    branches match {
      case Seq((e, s)) => Branch(Seq((stratifyExpr(e), dispatch(s))))
      case (e, s) +: (otherYes +: branches) =>
        Branch(Seq(
          (stratifyExpr(e), dispatch(s)),
          (tt, unfoldBranch(otherYes +: branches)),
        )) /* (blame) */
      case _ => ???
    }

  def stratifyExpr(e: Expr[Pre]): Expr[Post] = {
    val exprs = {
      // Ensure the "true" expression is kept
      val es = unfoldStar(e)
      if (es.isEmpty)
        Seq(e)
      else
        es
    }
    foldAny(e.t)(
      exprs.map {
        case e: ChorExpr[Pre] => (None, e)
        case e: ChorPerm[Pre] => (None, e)
        case expr => point(expr)
      }.map {
        case (Some(endpoint), expr) =>
          EndpointExpr[Post](succ(endpoint), dispatch(expr))(expr.o)
        case (None, expr) => dispatch(expr)
      }
    )(e.o)
  }

  // "Points" an expression in the direction of an endpoint if possible
  def point(e: Expr[Pre]): (Option[Endpoint[Pre]], Expr[Pre]) = {
    InferEndpointContexts.getEndpoints(e) match {
      case Seq(endpoint) =>
        // expr is totally in context of one endpoint and whatever else is in scope
        (Some(endpoint), e)
      case Seq() => (None, e)
      case _ =>
        // Expr uses multiple endpoints - for now we should disallow that.
        throw MultipleEndpoints(e)
    }
  }
}
