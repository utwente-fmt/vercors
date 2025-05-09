package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.{SystemError, UserError}
import vct.rewrite.veymont.InferEndpointContexts.{
  EndpointInferenceUndefined,
  getEndpoint,
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

  def getEndpoints[G](expr: Expr[G]): Seq[Endpoint[G]] =
    mutable.LinkedHashSet.from(expr.collect {
      case EndpointName(Ref(endpoint)) => endpoint
    }).toSeq

  def getEndpoint[G](expr: Expr[G]): Endpoint[G] =
    getEndpoints(expr) match {
      case Seq(endpoint) => endpoint
      case Seq() => throw NoImplicitEndpoint(expr)
      case _ => throw MultipleImplicitEndpoints(expr)
    }

  def getEndpoint[G](
      reportLocation: Node[_],
      exprs: Seq[Expr[G]],
  ): Endpoint[G] =
    exprs.flatMap(getEndpoints).distinct match {
      case Seq(endpoint) => endpoint
      case Seq() => throw NoImplicitEndpoint(reportLocation)
      case _ => throw MultipleImplicitEndpoints(reportLocation)
    }

  def getEndpoint[G](loc: Location[G]): Endpoint[G] =
    loc match {
      case FieldLocation(obj, _) => getEndpoint(obj)
      case AmbiguousLocation(deref) => getEndpoint(deref)
      case PredicateLocation(inv) =>
        inv match {
          case PredicateApply(ref, args) => getEndpoint(loc, args)
          case InstancePredicateApply(obj, ref, args) =>
            getEndpoint(loc, obj +: args)
          case CoalesceInstancePredicateApply(obj, ref, args) =>
            getEndpoint(loc, obj +: args)
        }
      case _ => throw EndpointInferenceUndefined(loc)
    }
}

case class InferEndpointContexts[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  val inChor = ScopedStack[Boolean]()
  val inEndpointExpr = ScopedStack[Endpoint[Pre]]()

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
          receiver = comm.receiver.map(_.decl)
            .orElse(Some(getEndpoint[Pre](comm.target)))
            .map(succ[Endpoint[Post]](_)),
          sender = comm.sender.map(_.decl)
            .orElse(Some(getEndpoint[Pre](comm.msg)))
            .map(succ[Endpoint[Post]](_)),
        ).succeed(comm)
      case _ => super.dispatch(decl)
    }

  override def dispatch(stmt: Statement[Pre]): Statement[Post] =
    stmt match {
      // Whitelist statements that do not need a context
      case s @ EndpointStatement(None, assign: Assign[Pre]) =>
        val endpoint: Endpoint[Pre] = getEndpoint(assign.target)
        s.rewrite(endpoint = Some(succ(endpoint)))
      case s @ EndpointStatement(None, Eval(invoke: MethodInvocation[Pre])) =>
        val endpoint: Endpoint[Pre] = getEndpoint(invoke.obj)
        s.rewrite(endpoint = Some(succ(endpoint)))
      case s @ EndpointStatement(None, _) => throw EndpointInferenceUndefined(s)
      case comm: CommunicateStatement[Pre] =>
        // Make inChor false because we don't want to infer endpoint contexts for expressions in the channel invariant
        // These should remain plain
        inChor.having(false) { comm.rewriteDefault() }
      case s => s.rewriteDefault()
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case expr @ EndpointExpr(Ref(endpoint), _) =>
        inEndpointExpr.having(endpoint) { expr.rewriteDefault() }
      case _ => expr.rewriteDefault()
    }
}
