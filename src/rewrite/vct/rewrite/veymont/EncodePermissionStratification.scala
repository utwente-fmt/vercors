package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  AllScopes,
  Assert,
  Assign,
  Block,
  ChorExpr,
  ChorPerm,
  ChorRun,
  ChorStatement,
  Choreography,
  Class,
  Communicate,
  CommunicateStatement,
  Declaration,
  Deref,
  Endpoint,
  EndpointExpr,
  EndpointName,
  EndpointStatement,
  Eval,
  Exhale,
  Expr,
  FieldLocation,
  Function,
  FunctionInvocation,
  InstanceField,
  InstanceMethod,
  Local,
  LocalDecl,
  Message,
  MethodInvocation,
  Node,
  Perm,
  Predicate,
  PredicateApply,
  PredicateLocation,
  Procedure,
  Program,
  Receiver,
  Scope,
  Sender,
  Statement,
  TClass,
  TVoid,
  ThisChoreography,
  Type,
  Unfolding,
  Value,
  Variable,
  WritePerm,
}
import vct.col.origin.{
  AssertFailed,
  AssignFailed,
  AssignLocalOk,
  Blame,
  CallableFailure,
  ChorAssignFailure,
  ContextEverywhereFailedInPost,
  ContextEverywhereFailedInPre,
  ContractedFailure,
  DiagnosticOrigin,
  EndpointContextEverywhereFailedInPre,
  EndpointPreconditionFailed,
  ExceptionNotInSignals,
  InsufficientPermission,
  InvocationFailure,
  Name,
  Origin,
  PanicBlame,
  ParticipantsNotDistinct,
  PostconditionFailed,
  PreconditionFailed,
  SeqAssignInsufficientPermission,
  SeqCallableFailure,
  SeqRunContextEverywhereFailedInPre,
  SeqRunPreconditionFailed,
  SignalsFailed,
  TerminationMeasureFailed,
  VerificationFailure,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}
import EncodeChoreography.{
  AssertFailedToParticipantsNotDistinct,
  AssignFailedToSeqAssignFailure,
  CallableFailureToSeqCallableFailure,
}
import vct.col.ref.Ref
import vct.rewrite.veymont

import scala.collection.{mutable => mut}

object EncodePermissionStratification extends RewriterBuilder {
  override def key: String = "encodePermissionStratification"
  override def desc: String =
    "Encodes stratification of permissions by wrapping each permission in an opaque predicate, guarding the permission using an endpoint reference."
}

case class EncodePermissionStratification[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {

  override def dispatch(program: Program[Pre]): Program[Post] = {
    mappings.program = program
    super.dispatch(program)
  }

  type WrapperPredicateKey = (Endpoint[Pre], Type[Pre], InstanceField[Pre])
  val wrapperPredicates = mut
    .LinkedHashMap[WrapperPredicateKey, Predicate[Post]]()

  def wrapperPredicate(
      endpoint: Endpoint[Pre],
      obj: Expr[Pre],
      field: InstanceField[Pre],
  )(implicit o: Origin): Ref[Post, Predicate[Post]] = {
    val k = (endpoint, obj.t, field)
    wrapperPredicates.getOrElseUpdate(
      k, {
        val endpointArg =
          new Variable(dispatch(endpoint.t))(o.where(name = "endpoint"))
        val objectArg = new Variable(dispatch(obj.t))(o.where(name = "obj"))
        val body = Perm[Post](
          FieldLocation(objectArg.get, succ(field)),
          WritePerm(),
        )
        new Predicate(Seq(endpointArg, objectArg), Some(body))(
          o.where(indirect =
            Name.names(Name("wrap"), field.o.getPreferredNameOrElse())
          )
        ).declare()
      },
    ).ref
  }

  val readFunctions = mut.LinkedHashMap[WrapperPredicateKey, Function[Post]]()
  def readFunction(
      endpoint: Endpoint[Pre],
      obj: Expr[Pre],
      field: InstanceField[Pre],
  )(implicit o: Origin): Ref[Post, Function[Post]] = {
    val k = (endpoint, obj.t, field)
    val pred = wrapperPredicate(endpoint, obj, field)
    readFunctions.getOrElseUpdate(
      k, {
        // pure field.t readField(endpoint.t endpoint, obj.t obj) = \unfolding wrapF(endpoint, obj) \in obj.field
        val endpointArg =
          new Variable(dispatch(endpoint.t))(o.where(name = "endpoint"))
        val objArg = new Variable(dispatch(obj.t))(o.where(name = "obj"))
        function(
          requires =
            Value(PredicateLocation(pred, Seq(endpointArg.get, objArg.get)))
              .accounted,
          args = Seq(endpointArg, objArg),
          returnType = dispatch(field.t),
          body = Some(
            Unfolding(
              Value(PredicateLocation(pred, Seq(endpointArg.get, objArg.get))),
              Deref[Post](objArg.get, succ(field))(PanicBlame("???")),
            )(PanicBlame("???"))
          ),
          blame = PanicBlame("???"),
          contractBlame = PanicBlame("???"),
        )(o.where(indirect =
          Name.names(Name("read"), field.o.getPreferredNameOrElse())
        )).declare()
      },
    ).ref
  }

  case class IdentityRewriter() extends Rewriter[Pre] {
    override val allScopes: AllScopes[Pre, Post] =
      EncodePermissionStratification.this.allScopes
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        implicit val o = chor.o
        currentChoreography.having(chor) {
          chor.rewrite(preRun =
            chor.preRun.map(Seq(_)).toSeq ++ Seq(
              unfoldPredicate(chor.run.contract.requires).map(
                Exhale(_)(PanicBlame(
                  "Exhaling non-stratified part of precondition failed"
                ))
              )(IdentityRewriter().dispatch(chor.run.contract.requires))()
            )
          ).succeed(chor)
        }
      case _ => super.dispatch(decl)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case InChor(_, ChorPerm(Ref(endpoint), loc: FieldLocation[Pre], perm)) =>
        implicit val o = expr.o
        PredicateApply(
          wrapperPredicate(endpoint, loc.obj, loc.field.decl)(expr.o),
          Seq(EndpointName(succ(endpoint)), dispatch(loc.obj)),
          dispatch(perm),
        )

      case EndpointExpr(Ref(endpoint), inner) =>
        assert(currentEndpoint.isEmpty)
        currentEndpoint.having(endpoint) { dispatch(inner) }

      case InEndpoint(_, endpoint, Deref(obj, Ref(field))) =>
        implicit val o = expr.o
        functionInvocation(
          ref = readFunction(endpoint, obj, field)(expr.o),
          args = Seq(EndpointName(succ(endpoint)), dispatch(obj)),
          blame = PanicBlame("???"),
        )

      case _ => expr.rewriteDefault()
    }
}
