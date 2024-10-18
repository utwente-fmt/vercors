package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.check.SeqProgParticipant
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.InferEndpointContexts.{
  MultipleImplicitEndpoints,
  NoImplicitEndpoint,
}
import vct.rewrite.veymont.StratifyExpressions.SeqProgParticipantErrors

object StratifyExpressions extends RewriterBuilder {
  override def key: String = "stratifyExpressions"
  override def desc: String =
    "Stratifies expressions by putting all contracts, branch conditions and loop conditions within a choreography's run declaration into endpoint exprs, inferring endpoint contexts where required."

  case class SeqProgParticipantErrors(es: Seq[SeqProgParticipant])
      extends UserError {
    override def code: String = "seqProgParticipantErrors"
    override def text: String =
      es.map { err: SeqProgParticipant => err.message { n => n.o } }
        .mkString("\n")
  }
}

case class StratifyExpressions[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {

  override def dispatch(prog: Program[Pre]): Program[Post] = {
    mappings.program = prog

    val newProg = prog.rewrite()
    val errors = newProg.check
    /* TODO (RR): if we refactor branches to be nested instead of flat, this check can
         happen directly after LangVeyMontToCol. Or we should consider putting the flattening in its own pass,
         which then also contains the below check */
    val seqBranchErrors = errors.collect { case err: SeqProgParticipant => err }
    if (errors.nonEmpty && errors.length == seqBranchErrors.length) {
      throw SeqProgParticipantErrors(seqBranchErrors)
    }
    newProg
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) { chor.rewriteDefault().succeed(chor) }
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

      case InChor(_, branch @ Branch(Seq((cond, yes)))) =>
        branch.rewrite(Seq((stratifyExpr(cond), dispatch(yes))))

      case InChor(
            _,
            branch @ Branch(Seq((cond, yes), (BooleanValue(true), no))),
          ) =>
        branch
          .rewrite(Seq((stratifyExpr(cond), dispatch(yes)), (tt, dispatch(no))))

      // We expect all branches to be normalized to binary branches
      case InChor(_, Branch(_)) => ???

      case InChor(_, assert: Assert[Pre]) =>
        assert.rewrite(res = stratifyExpr(assert.expr))
      case InChor(_, inhale: Inhale[Pre]) =>
        inhale.rewrite(res = stratifyExpr(inhale.expr))
      case InChor(_, exhale: Exhale[Pre]) =>
        exhale.rewrite(res = stratifyExpr(exhale.expr))
      case InChor(_, assume: Assume[Pre]) =>
        assume.rewrite(assn = stratifyExpr(assume.expr))

      case statement => statement.rewriteDefault()
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
        case e: EndpointExpr[Pre] => (None, e)
        case expr => point(expr)
      }.map {
        case (Some(endpoint), expr) =>
          EndpointExpr[Post](succ(endpoint), dispatch(expr))(expr.o)
        case (None, expr) => expr.rewriteDefault()
      }
    )(e.o)
  }

  // "Points" an expression in the direction of an endpoint if possible
  def point(e: Expr[Pre]): (Option[Endpoint[Pre]], Expr[Pre]) = {
    InferEndpointContexts.getEndpoints(e) match {
      case Seq(endpoint) =>
        // expr is totally in context of one endpoint and whatever else is in scope
        (Some(endpoint), e)
      // Expressions of type resource _must_ be pointed
      case Seq() if e.t == TResource[Pre]() => throw NoImplicitEndpoint(e)
      // Other expressions, presumably bool-typed, will be duplicated in stratifyUnpointedExpressions
      case Seq() => (None, e)
      case _ =>
        // Expr uses multiple endpoints - for now we should disallow that.
        throw MultipleImplicitEndpoints(e)
    }
  }
}
