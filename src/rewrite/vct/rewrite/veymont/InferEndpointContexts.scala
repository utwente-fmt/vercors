package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.{SystemError, UserError}
import vct.rewrite.veymont.InferEndpointContexts.{
  EndpointInferenceUndefined,
  getTarget,
}

import scala.collection.mutable

object InferEndpointContexts extends RewriterBuilder {
  override def key: String = "inferEndpointContexts"
  override def desc: String =
    "Infer endpoint context for nodes where the user could have put in manual annotations but did not: assignment, method invocations, and communicate."

  case class NoImplicitEndpoint(expr: Node[_]) extends UserError {
    override def code: String = "noImplicitEndpoint"
    override def text: String =
      expr.o.messageInContext(
        "Cannot infer an endpoint context for this expression."
      )
  }

  case class MultipleImplicitEndpoints(expr: Node[_]) extends UserError {
    override def code: String = "multipleImplicitEndpoints"
    override def text: String =
      expr.o.messageInContext(
        "This expression references multiple distinct endpoints, whereas only one is expected."
      )
  }

  case class EndpointInferenceUndefined(stmt: Node[_]) extends SystemError {
    override def text: String =
      stmt.o.messageInContext(
        "It is not defined whether an endpoint context should be inferred for this node"
      )
  }

  def getTargets[G](expr: Expr[G]): Seq[CommunicateTarget[G]] =
    mutable.LinkedHashSet.from(expr.collect { case CtExpr(target) => target })
      .toSeq

  def getTarget[G](expr: Expr[G]): CommunicateTarget[G] =
    getTargets(expr) match {
      case Seq(target) => target
      case Seq() => throw NoImplicitEndpoint(expr)
      case _ => throw MultipleImplicitEndpoints(expr)
    }

  def getTarget[G](
      reportLocation: Node[_],
      exprs: Seq[Expr[G]],
  ): CommunicateTarget[G] =
    exprs.flatMap(getTargets).distinct match {
      case Seq(endpoint) => endpoint
      case Seq() => throw NoImplicitEndpoint(reportLocation)
      case _ => throw MultipleImplicitEndpoints(reportLocation)
    }

  def getTarget[G](loc: Location[G]): CommunicateTarget[G] =
    loc match {
      case FieldLocation(obj, _) => getTarget(obj)
      case AmbiguousLocation(deref) => getTarget(deref)
      case PredicateLocation(inv) =>
        inv match {
          case PredicateApply(ref, args) => getTarget(loc, args)
          case InstancePredicateApply(obj, ref, args) =>
            getTarget(loc, obj +: args)
          case CoalesceInstancePredicateApply(obj, ref, args) =>
            getTarget(loc, obj +: args)
        }
      case _ => throw EndpointInferenceUndefined(loc)
    }
}

case class InferEndpointContexts[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  val inChor = ScopedStack[Boolean]()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        chor.rewrite(
          preRun = inChor.having(true) { chor.preRun.map(dispatch) },
          run = inChor.having(true) { dispatch(chor.run) },
        ).succeed(chor)
      case comm: Communicate[Pre] =>
        implicit val o = comm.o
        comm.rewrite(
          receiver = comm.receiver
            .orElse(Some(getTarget[Pre](comm.destination))).map(dispatch),
          sender = comm.sender.orElse(Some(getTarget[Pre](comm.msg)))
            .map(dispatch),
        ).succeed(comm)
      case _ => super.dispatch(decl)
    }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] =
    stmt match {
      // Whitelist statements for which we can try and infer an endpoint context
      case s @ EndpointStatement(None, assign: Assign[Pre]) =>
        s.rewrite(endpoint = Some(dispatch(getTarget(assign.target))))
      case s @ EndpointStatement(None, Eval(invoke: MethodInvocation[Pre])) =>
        s.rewrite(endpoint = Some(dispatch(getTarget(invoke.obj))))
      case s @ EndpointStatement(None, _) => throw EndpointInferenceUndefined(s)
      case comm: CommunicateStatement[Pre] =>
        // Make inChor false because we don't want to infer endpoint contexts for expressions in the channel invariant
        // These should remain plain
        inChor.having(false) { comm.rewriteDefault() }
      case s => s.rewriteDefault()
    }
}
