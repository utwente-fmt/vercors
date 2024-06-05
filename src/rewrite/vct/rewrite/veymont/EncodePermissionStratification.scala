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
  Fold,
  Function,
  FunctionInvocation,
  Inhale,
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
  Unfold,
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

  type WrapperPredicateKey = (TClass[Pre], Type[Pre], InstanceField[Pre])
  val wrapperPredicates = mut
    .LinkedHashMap[WrapperPredicateKey, Predicate[Post]]()

  def wrapperPredicate(
      endpoint: Endpoint[Pre],
      obj: Expr[Pre],
      field: InstanceField[Pre],
  )(implicit o: Origin): Ref[Post, Predicate[Post]] = {
    val k = (endpoint.t, obj.t, field)
    wrapperPredicates.getOrElseUpdate(
      k, {
        logger.debug(s"Declaring wrapper predicate for $k")
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
    val k = (endpoint.t, obj.t, field)
    val pred = wrapperPredicate(endpoint, obj, field)
    readFunctions.getOrElseUpdate(
      k, {
        logger.debug(s"Declaring read function for $k")
        val endpointArg =
          new Variable(dispatch(endpoint.t))(o.where(name = "endpoint"))
        val objArg = new Variable(dispatch(obj.t))(o.where(name = "obj"))
        function(
          requires =
            Value(PredicateLocation(pred, Seq(endpointArg.get, objArg.get))(
              o.where(context = "precondition")
            )).accounted,
          args = Seq(endpointArg, objArg),
          returnType = dispatch(field.t),
          body = Some(
            Unfolding(
              Value(PredicateLocation(pred, Seq(endpointArg.get, objArg.get))(
                o.where(context = "body")
              )),
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

  case class StripChorPerm() extends Rewriter[Pre] {
    override val allScopes: AllScopes[Pre, Post] =
      EncodePermissionStratification.this.allScopes

    override def dispatch(expr: Expr[Pre]): Expr[Post] =
      expr match {
        case ChorPerm(_, loc, perm) =>
          Perm(dispatch(loc), dispatch(perm))(expr.o)
        case _ => expr.rewriteDefault()
      }
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        implicit val o = chor.o
        currentChoreography.having(chor) {
          chor.rewrite(preRun =
            Some(Block(
              chor.preRun.map(dispatch).toSeq ++
                (Seq(
                  Exhale[Post](StripChorPerm().dispatch(foldStar(
                    unfoldPredicate(chor.run.contract.requires)
                  )))(PanicBlame(
                    "Exhaling non-stratified part of precondition failed"
                  ))
                )) ++
                (unfoldPredicate(chor.run.contract.requires)
                  .map(e => Inhale[Post](dispatch(e))))
            ))
          ).succeed(chor)
        }
      case _ => super.dispatch(decl)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case InChor(_, cp: ChorPerm[Pre]) =>
        assert(currentEndpoint.isEmpty)
        currentEndpoint.having(cp.endpoint.decl) { dispatch(cp) }

      case InEndpoint(
            _,
            _,
            ChorPerm(Ref(endpoint), loc: FieldLocation[Pre], perm),
          ) =>
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

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case EndpointStatement(
            Some(Ref(endpoint)),
            assign @ Assign(target @ Deref(obj, Ref(field)), _),
          ) =>
        implicit val o = statement.o
        val apply = {
          val newEndpoint: Ref[Post, Endpoint[Post]] = succ(endpoint)
          val ref = wrapperPredicate(endpoint, obj, field)
          PredicateApply[Post](
            ref,
            Seq(EndpointName(newEndpoint), dispatch(obj)),
            WritePerm(),
          )
        }
        val intermediate =
          new Variable(dispatch(assign.value.t))(
            assign.o.where(name = "intermediate")
          )
        Scope(
          Seq(intermediate),
          Block(Seq(
            currentEndpoint.having(endpoint) {
              assignLocal(intermediate.get, dispatch(assign.value))
            },
            Unfold(apply)(PanicBlame("TODO: Use blame on endpoint")),
            assign.rewrite(value = intermediate.get),
            Fold(apply)(PanicBlame("TODO: Use blame on endpointstatement")),
          )),
        )
      case EndpointStatement(Some(Ref(endpoint)), assert: Assert[Pre]) =>
        currentEndpoint.having(endpoint) { assert.rewriteDefault() }
      case EndpointStatement(_, eval: Eval[Pre]) =>
        throw new Exception(statement.o.messageInContext(
          "Eval with permission stratification not yet supported"
        ))
      case _ => statement.rewriteDefault()
    }
}
