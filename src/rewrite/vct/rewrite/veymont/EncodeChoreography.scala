package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
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
  Expr,
  InstanceMethod,
  Local,
  LocalDecl,
  Message,
  MethodInvocation,
  Node,
  Perm,
  Procedure,
  Receiver,
  Scope,
  Sender,
  Statement,
  TClass,
  TVoid,
  ThisChoreography,
  Variable,
}
import vct.col.origin.{
  AssertFailed,
  AssignFailed,
  AssignLocalOk,
  Blame,
  CallableFailure,
  ChorAssignFailure,
  ChorRunContextEverywhereFailedInPre,
  ChorRunFailure,
  ChorRunPreconditionFailed,
  ChoreographyFailure,
  ContextEverywhereFailedInPost,
  ContextEverywhereFailedInPre,
  ContractedFailure,
  DiagnosticOrigin,
  ExceptionNotInSignals,
  InsufficientPermission,
  InvocationFailure,
  Origin,
  PanicBlame,
  ParticipantsNotDistinct,
  PostconditionFailed,
  PreconditionFailed,
  SeqAssignInsufficientPermission,
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
}
import vct.col.ref.Ref
import vct.rewrite.veymont

import scala.collection.{mutable => mut}

object EncodeChoreography extends RewriterBuilder {
  override def key: String = "encodeChoreography"
  override def desc: String = "Encodes the semantics of a VeyMont choreography."

  object SignalsAlwaysEmpty extends PanicBlame("signals always empty")

  case class CallableFailureToContractedFailure(blame: Blame[ContractedFailure])
      extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit =
      error match {
        case failure: ContractedFailure => blame.blame(failure)
        case SignalsFailed(failure, node) => SignalsAlwaysEmpty.blame(error)
        case ExceptionNotInSignals(node) => SignalsAlwaysEmpty.blame(error)
      }
  }

  case class AssignFailedToSeqAssignFailure(assign: EndpointStatement[_])
      extends Blame[AssignFailed] {
    override def blame(error: AssignFailed): Unit =
      assign.blame.blame(SeqAssignInsufficientPermission(assign))
  }

  case class AssertFailedToParticipantsNotDistinct(comm: Communicate[_])
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      comm.blame.blame(ParticipantsNotDistinct(comm))
  }

  case class InvocationFailureToChorRunFailure(run: ChorRun[_])
      extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(path, fail, node) =>
          run.blame.blame(ChorRunPreconditionFailed(Some(path), fail, run))
        case ContextEverywhereFailedInPre(fail, node) =>
          run.blame.blame(ChorRunContextEverywhereFailedInPre(fail, run))
      }
  }
}

case class EncodeChoreography[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  import EncodeChoreography._

  val currentProg: ScopedStack[Choreography[Pre]] = ScopedStack()
  val currentRun: ScopedStack[ChorRun[Pre]] = ScopedStack()
  val currentInstanceMethod: ScopedStack[InstanceMethod[Pre]] = ScopedStack()
  val currentCommunicate: ScopedStack[Communicate[Pre]] = ScopedStack()

  sealed trait Mode
  case object Top extends Mode
  case class InProg(prog: Choreography[Pre]) extends Mode
  case class InRun(prog: Choreography[Pre], run: ChorRun[Pre]) extends Mode
  case class InMethod(prog: Choreography[Pre], method: InstanceMethod[Pre])
      extends Mode

  def mode: Mode =
    (
      currentProg.topOption,
      currentRun.topOption,
      currentInstanceMethod.topOption,
    ) match {
      case (None, None, None) => Top
      case (Some(prog), None, None) => InProg(prog)
      case (Some(prog), Some(run), None) => InRun(prog, run)
      case (Some(prog), None, Some(method)) => InMethod(prog, method)
      case (None, None, Some(_)) => Top
      case (_, _, _) =>
        throw Unreachable("AST structure should prevent this case")
    }

  val runSucc: mut.Map[ChorRun[Pre], Procedure[Post]] = mut.LinkedHashMap()
  val progSucc: SuccessionMap[Choreography[Pre], Procedure[Post]] =
    SuccessionMap()
  val methodSucc: SuccessionMap[InstanceMethod[Pre], Procedure[Post]] =
    SuccessionMap()
  val endpointSucc: SuccessionMap[(Mode, Endpoint[Pre]), Variable[Post]] =
    SuccessionMap()
  val variableSucc: SuccessionMap[(Mode, Variable[Pre]), Variable[Post]] =
    SuccessionMap()
  val msgSucc: SuccessionMap[Communicate[Pre], Variable[Post]] = SuccessionMap()

  override def dispatch(decl: Declaration[Pre]): Unit =
    (mode, decl) match {
      case (Top, prog: Choreography[Pre]) =>
        currentProg.having(prog) {
          // First generate a procedure that implements the run method
          rewriteRun(prog)

          // And also process all auxiliary instance methods
          prog.decls.foreach(dispatch)

          // Then generate a procedure that initializes all the endpoints and calls the run procedure
          // First set up the succesor variables that will be encoding the seq_program argument and endpoints
          implicit val o = prog.o
          prog.endpoints.foreach(_.drop())
          for (endpoint <- prog.endpoints) {
            endpointSucc((mode, endpoint)) =
              new Variable(dispatch(endpoint.t))(endpoint.o)
          }

          // Maintain successor for seq_prog argument variables manually, as two contexts are maintained
          // The main procedure context and run procedure contex
          prog.params.foreach(_.drop())
          for (arg <- prog.params) {
            variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
          }

          // For each endpoint, make a local variable and initialize it using the constructor referenced in the endpoint
          val endpointsInit = prog.endpoints.map { endpoint =>
            Assign(
              Local[Post](endpointSucc((mode, endpoint)).ref),
              dispatch(endpoint.init),
            )(AssignLocalOk)
          }

          val preRun = prog.preRun.map(dispatch).toSeq

          // Invoke the run procedure with the seq_program arguments, as well as all the endpoints
          val invokeRun = Eval(procedureInvocation[Post](
            ref = runSucc(prog.run).ref,
            args =
              prog.params
                .map(arg => Local[Post](variableSucc((mode, arg)).ref)) ++
                prog.endpoints.map(endpoint =>
                  Local[Post](endpointSucc((mode, endpoint)).ref)
                ),
            blame = InvocationFailureToChorRunFailure(prog.run),
          ))

          // Scope the endpoint vars and combine initialization and run method invocation
          val body = Scope(
            prog.endpoints.map(endpoint => endpointSucc((mode, endpoint))),
            Block((endpointsInit ++ preRun) :+ invokeRun),
          )

          progSucc(prog) = globalDeclarations.declare(
            new Procedure(
              returnType = TVoid(),
              outArgs = Seq(),
              typeArgs = Seq(),
              args = prog.params.map(arg => variableSucc((mode, arg))),
              contract = dispatch(prog.contract),
              body = Some(body),
            )(CallableFailureToContractedFailure(prog.blame))
          )
        }

      case (InProg(prog), method: InstanceMethod[Pre]) =>
        currentInstanceMethod.having(method) {
          for (endpoint <- prog.endpoints) {
            endpointSucc((mode, endpoint)) =
              new Variable(TClass(succ[Class[Post]](endpoint.cls.decl), Seq()))(
                endpoint.o
              )
          }

          prog.params.foreach(_.drop())
          for (arg <- prog.params) {
            variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
          }

          methodSucc(method) = globalDeclarations.declare(
            new Procedure(
              args =
                prog.params.map(arg => variableSucc((mode, arg))) ++
                  prog.endpoints
                    .map(endpoint => endpointSucc((mode, endpoint))),
              body = method.body.map(dispatch),
              outArgs = Nil,
              typeArgs = Nil,
              returnType = dispatch(method.returnType),
              contract = dispatch(method.contract),
            )(method.blame)(method.o)
          )
        }

      case _ => rewriteDefault(decl)
    }

  def rewriteRun(prog: Choreography[Pre]): Unit = {
    val run = prog.run
    implicit val o: Origin = run.o
      .where(name = currentProg.top.o.getPreferredNameOrElse().snake + "_run")

    currentRun.having(run) {
      for (endpoint <- prog.endpoints) {
        endpointSucc((mode, endpoint)) =
          new Variable(dispatch(endpoint.t))(endpoint.o)
      }

      for (arg <- prog.params) {
        variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
      }

      runSucc(run) = globalDeclarations.declare(
        new Procedure(
          args =
            prog.params.map(arg => variableSucc((mode, arg))) ++
              prog.endpoints.map(endpoint => endpointSucc((mode, endpoint))),
          contract = dispatch(run.contract),
          body = Some(dispatch(run.body)),
          outArgs = Seq(),
          typeArgs = Seq(),
          returnType = TVoid(),
        )(CallableFailureToContractedFailure(run.blame))
      )
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case assign @ EndpointStatement(None, Assign(target, e)) =>
        throw new Exception(
          assign.o.messageInContext("ChorStatement with None!")
        )
      case assign @ EndpointStatement(Some(Ref(endpoint)), Assign(target, e)) =>
        logger
          .warn(s"Ignoring endpoint annotation on chor assign statement ${assign
              .o.shortPosition.map("at " + _).getOrElse("")}")
        implicit val o = assign.o
        if (endpoint != InferEndpointContexts.getEndpoint(target)) {
          throw new Exception(assign.o.messageInContext(
            "Assign endpoint in value does not match endpoint that was annotated or inferred"
          ))
        }
        Assign(dispatch(target), dispatch(e))(AssignFailedToSeqAssignFailure(
          assign
        ))
      case EndpointStatement(_, stat) => dispatch(stat)
      case s @ ChorStatement(inner) =>
        logger.warn(
          s"Ignoring semantics of ChorStatement at ${s.o.shortPositionText}"
        )
        dispatch(inner)
      case ep @ EndpointStatement(_, inner) =>
        throw new Exception(ep.o.messageInContext(
          "TODO: Implement choreographic verification encoding for this"
        ))
      case stat => rewriteDefault(stat)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    (mode, expr) match {
      case (mode, EndpointName(Ref(endpoint))) =>
        endpointSucc((mode, endpoint)).get(expr.o)
      case (mode, Local(Ref(v)))
          if mode != Top && currentProg.top.params.contains(v) =>
        variableSucc((mode, v)).get(expr.o)
      case (
            mode,
            invocation @ MethodInvocation(
              ThisChoreography(_),
              Ref(method),
              args,
              _,
              _,
              _,
              _,
            ),
          ) if mode != Top =>
        implicit val o = invocation.o
        val prog = currentProg.top
        assert(args.isEmpty)
        procedureInvocation(
          ref = methodSucc.ref(method),
          args =
            prog.params
              .map(arg => Local[Post](variableSucc.ref((mode, arg)))(arg.o)) ++
              prog.endpoints.map(endpoint =>
                Local[Post](endpointSucc.ref((mode, endpoint)))(invocation.o)
              ),
          blame = invocation.blame,
        )
      case (mode, p @ ChorPerm(Ref(endpoint), loc, perm)) =>
        if (endpoint != InferEndpointContexts.getEndpoint(loc)) {
          throw new Exception(p.o.messageInContext(
            "Endpoint in obj does not match inferred/annotated endpoint"
          ))
        }
        Perm(dispatch(loc), dispatch(perm))(p.o)
      case (mode, Sender(Ref(comm))) =>
        implicit val o = expr.o
        endpointSucc((mode, comm.sender.get.decl)).get
      case (mode, Receiver(Ref(comm))) =>
        implicit val o = expr.o
        endpointSucc((mode, comm.receiver.get.decl)).get
      case (mode, Message(Ref(comm))) =>
        implicit val o = expr.o
        msgSucc(comm).get
      case (mode, EndpointExpr(Ref(endpoint), expr)) =>
        logger.warn(
          "Ignoring endpoint expr annotation at " + expr.o.shortPositionText
        )
        dispatch(expr)
      case (mode, ChorExpr(inner)) => dispatch(inner)
      case (_, expr) => expr.rewriteDefault()
    }
}
