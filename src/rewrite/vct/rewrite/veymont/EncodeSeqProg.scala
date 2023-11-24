package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{Access, Assign, Block, Class, Communicate, Declaration, Deref, Endpoint, EndpointName, EndpointUse, Eval, Expr, InstanceMethod, Local, LocalDecl, MethodInvocation, Node, Procedure, Scope, SeqAssign, SeqProg, SeqRun, Statement, Subject, TClass, TVoid, ThisSeqProg, Variable}
import vct.col.origin.{AccessFailure, AccessInsufficientPermission, AssignFailed, AssignLocalOk, Blame, CallableFailure, ContextEverywhereFailedInPost, ContextEverywhereFailedInPre, ContractedFailure, DiagnosticOrigin, EndpointContextEverywhereFailedInPre, EndpointPreconditionFailed, ExceptionNotInSignals, InsufficientPermission, InvocationFailure, Origin, PanicBlame, PostconditionFailed, PreconditionFailed, SeqAssignFailure, SeqAssignInsufficientPermission, SeqCallableFailure, SeqRunContextEverywhereFailedInPre, SeqRunPreconditionFailed, SignalsFailed, TerminationMeasureFailed, VerificationFailure}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}
import vct.col.ref.Ref

import scala.collection.{mutable => mut}

object EncodeSeqProg extends RewriterBuilder {
  override def key: String = "encodeSeqProg"
  override def desc: String = "Encodes the semantics of a parallel VeyMont program."

  object SignalsAlwaysEmpty extends PanicBlame("signals always empty")

  case class CallableFailureToSeqCallableFailure(seqBlame: Blame[SeqCallableFailure]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case failure: SeqCallableFailure => seqBlame.blame(failure)
      case SignalsFailed(failure, node) => SignalsAlwaysEmpty.blame(error)
      case ExceptionNotInSignals(node) => SignalsAlwaysEmpty.blame(error)
    }
  }

  case class InsufficientPermissionToAccessFailure(access: Access[_]) extends Blame[VerificationFailure] {
    override def blame(error: VerificationFailure): Unit = error match {
      case _: AssignFailed =>
        access.blame.blame(AccessInsufficientPermission(access))
      case _: InsufficientPermission =>
        access.blame.blame(AccessInsufficientPermission(access))
      case _ => PanicBlame("Error should either be AssignFailed or InsufficientPermission").blame(error)
    }
  }

  case class AssignFailedToSeqAssignFailure(assign: SeqAssign[_]) extends Blame[AssignFailed] {
    override def blame(error: AssignFailed): Unit =
      assign.blame.blame(SeqAssignInsufficientPermission(assign))
  }

  case class ToSeqRunFailure(run: SeqRun[_]) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit = error match {
      case PreconditionFailed(path, failure, node) => run.blame.blame(SeqRunPreconditionFailed(path, failure, run))
      case ContextEverywhereFailedInPre(failure, node) => run.blame.blame(SeqRunContextEverywhereFailedInPre(failure, run))
    }
  }

  case class InvocationFailureToEndpointFailure(endpoint: Endpoint[_]) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit = error match {
      case PreconditionFailed(path, failure, _) => endpoint.blame.blame(EndpointPreconditionFailed(path, failure, endpoint))
      case ContextEverywhereFailedInPre(failure, _) => endpoint.blame.blame(EndpointContextEverywhereFailedInPre(failure, endpoint))
    }
  }
}

case class EncodeSeqProg[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  import EncodeSeqProg._

  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()
  val currentRun: ScopedStack[SeqRun[Pre]] = ScopedStack()
  val currentInstanceMethod: ScopedStack[InstanceMethod[Pre]] = ScopedStack()

  sealed trait Mode
  case object Top extends Mode
  case class InProg(prog: SeqProg[Pre]) extends Mode
  case class InRun(prog: SeqProg[Pre], run: SeqRun[Pre]) extends Mode
  case class InMethod(prog: SeqProg[Pre], method: InstanceMethod[Pre]) extends Mode

  def mode: Mode = (currentProg.topOption, currentRun.topOption, currentInstanceMethod.topOption) match {
    case (None, None, None) => Top
    case (Some(prog), None, None) => InProg(prog)
    case (Some(prog), Some(run), None) => InRun(prog, run)
    case (Some(prog), None, Some(method)) => InMethod(prog, method)
    case (None, None, Some(_)) => Top
    case (_, _, _) => throw Unreachable("AST structure should prevent this case")
  }

  val runSucc: mut.Map[SeqRun[Pre], Procedure[Post]] = mut.LinkedHashMap()
  val progSucc: SuccessionMap[SeqProg[Pre], Procedure[Post]] = SuccessionMap()
  val methodSucc: SuccessionMap[InstanceMethod[Pre], Procedure[Post]] = SuccessionMap()
  val endpointSucc: SuccessionMap[(Mode, Endpoint[Pre]), Variable[Post]] = SuccessionMap()
  val variableSucc: SuccessionMap[(Mode, Variable[Pre]), Variable[Post]] = SuccessionMap()

  override def dispatch(decl: Declaration[Pre]): Unit = (mode, decl) match {
    case (Top, prog: SeqProg[Pre]) => currentProg.having(prog) {
      // First generate a procedure that implements the run method
      rewriteRun(prog)

      // And also process all auxiliary instance methods
      prog.decls.foreach(dispatch)

      // Then generate a procedure that initializes all the endpoints and calls the run procedure
      // First set up the succesor variables that will be encoding the seq_program argument and endpoints
      implicit val o = prog.o
      prog.endpoints.foreach(_.drop())
      for (endpoint <- prog.endpoints) {
        endpointSucc((mode, endpoint)) = new Variable(TClass(succ[Class[Post]](endpoint.cls.decl)))(endpoint.o)
      }

      // Maintain successor for seq_prog argument variables manually, as two contexts are maintained
      // The main procedure context and run procedure contex
      prog.args.foreach(_.drop())
      for(arg <- prog.args) {
        variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
      }

      // For each endpoint, make a local variable and initialize it using the constructor referenced in the endpoint
      val endpointsInit = prog.endpoints.map { endpoint =>
        Assign(Local[Post](endpointSucc((mode, endpoint)).ref),
          procedureInvocation[Post](
            ref = succ(endpoint.constructor.decl),
            args = endpoint.args.map(dispatch),
            blame = InvocationFailureToEndpointFailure(endpoint)
          )
        )(AssignLocalOk)
      }

      // Invoke the run procedure with the seq_program arguments, as well as all the endpoints
      val invokeRun = Eval(procedureInvocation[Post](
        ref = runSucc(prog.run).ref,
        args = prog.args.map(arg => Local[Post](variableSucc((mode, arg)).ref)) ++
          prog.endpoints.map(endpoint => Local[Post](endpointSucc((mode, endpoint)).ref)),
        blame = ToSeqRunFailure(prog.run)
      ))

      // Scope the endpoint vars and combine initialization and run method invocation
      val body = Scope(
        prog.endpoints.map(endpoint => endpointSucc((mode, endpoint))),
        Block(endpointsInit :+ invokeRun)
      )

      progSucc(prog) = globalDeclarations.declare(new Procedure(
        returnType = TVoid(), outArgs = Seq(), typeArgs = Seq(),
        args = prog.args.map(arg => variableSucc((mode, arg))),
        contract = dispatch(prog.contract),
        body = Some(body)
      )(CallableFailureToSeqCallableFailure(prog.blame)))
    }

    case (InProg(prog), method: InstanceMethod[Pre]) => currentInstanceMethod.having(method) {
      for (endpoint <- prog.endpoints) {
        endpointSucc((mode, endpoint)) = new Variable(TClass(succ[Class[Post]](endpoint.cls.decl)))(endpoint.o)
      }

      prog.args.foreach(_.drop())
      for (arg <- prog.args) {
        variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
      }

      methodSucc(method) = globalDeclarations.declare(new Procedure(
        args = prog.args.map(arg => variableSucc((mode, arg))) ++
          prog.endpoints.map(endpoint => endpointSucc((mode, endpoint))),
        body = method.body.map(dispatch),
        outArgs = Nil, typeArgs = Nil, returnType = dispatch(method.returnType), contract = dispatch(method.contract)
      )(method.blame)(method.o))
    }

    case _ => rewriteDefault(decl)
  }

  def rewriteRun(prog: SeqProg[Pre]): Unit = {
    val run = prog.run
    implicit val o: Origin = run.o.where(name = currentProg.top.o.getPreferredNameOrElse().snake + "_run")

    currentRun.having(run) {
      for (endpoint <- prog.endpoints) {
        endpointSucc((mode, endpoint)) = new Variable(TClass(succ[Class[Post]](endpoint.cls.decl)))(endpoint.o)
      }

      for (arg <- prog.args) {
        variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
      }

      runSucc(run) = globalDeclarations.declare(new Procedure(
        args = prog.args.map(arg => variableSucc((mode, arg))) ++
          prog.endpoints.map(endpoint => endpointSucc((mode, endpoint))),
        contract = dispatch(run.contract),
        body = Some(dispatch(run.body)),
        outArgs = Seq(), typeArgs = Seq(),
        returnType = TVoid(),
      )(CallableFailureToSeqCallableFailure(run.blame)))
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case assign@SeqAssign(Ref(endpoint), Ref(field), e) =>
      implicit val o = assign.o
      Assign(
        Deref[Post](
          Local(endpointSucc((mode, endpoint)).ref),
          succ(field)
        )(PanicBlame("Unused by Silver encoding")),
        dispatch(e)
      )(AssignFailedToSeqAssignFailure(assign))
    case comm @ Communicate(receiver, sender) =>
      implicit val o = comm.o
      Assign[Post](
        rewriteAccess(receiver),
        rewriteAccess(sender)
      )(InsufficientPermissionToAccessFailure(receiver))
    case stat => rewriteDefault(stat)
  }

  def rewriteAccess(access: Access[Pre]): Expr[Post] =
    Deref[Post](rewriteSubject(access.subject), succ(access.field.decl))(InsufficientPermissionToAccessFailure(access))(access.o)

  def rewriteSubject(subject: Subject[Pre]): Expr[Post] = subject match {
    case EndpointName(Ref(endpoint)) => Local[Post](endpointSucc((mode, endpoint)).ref)(subject.o)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = (mode, expr) match {
    case (mode, EndpointUse(Ref(endpoint))) =>
      Local[Post](endpointSucc((mode, endpoint)).ref)(expr.o)
    case (mode, Local(Ref(v))) if mode != Top && currentProg.top.args.contains(v) =>
      Local[Post](variableSucc((mode, v)).ref)(expr.o)
    case (mode, invocation @ MethodInvocation(ThisSeqProg(_), Ref(method), args, _, _, _, _)) if mode != Top =>
      implicit val o = invocation.o
      val prog = currentProg.top
      assert(args.isEmpty)
      procedureInvocation(
        ref = methodSucc.ref(method),
        args = prog.args.map(arg => Local[Post](variableSucc.ref((mode, arg)))(arg.o)) ++
          prog.endpoints.map(endpoint => Local[Post](endpointSucc.ref((mode, endpoint)))(invocation.o)),
        blame = invocation.blame
      )
    case (_, expr) => rewriteDefault(expr)
  }
}
