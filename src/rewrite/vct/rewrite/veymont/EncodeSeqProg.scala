package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{Access, Assign, Block, Class, Communicate, Declaration, Deref, Endpoint, EndpointName, EndpointUse, Eval, Expr, Local, LocalDecl, Node, Procedure, Scope, SeqAssign, SeqProg, SeqRun, Statement, Subject, TClass, TVoid, Variable}
import vct.col.origin.{AccessFailure, AccessInsufficientPermission, AssignFailed, Blame, CallableFailure, DiagnosticOrigin, InsufficientPermission, Origin, PanicBlame, SeqAssignFailure, SeqAssignInsufficientPermission, VerificationFailure}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import EncodeSeqProg.{CallableFailureNotSupportedBlame, CommunicateNotSupported, GeneratedPerm, SeqAssignNotSupported}
import vct.col.ref.Ref

import scala.collection.{mutable => mut}

object EncodeSeqProg extends RewriterBuilder {
  override def key: String = "encodeSeqProg"
  override def desc: String = "Encodes the semantics of a parallel VeyMont program."

  case object CommunicateNotSupported extends UserError {
    override def code: String = "communicateNotSupported"
    override def text: String = "The `communicate` statement is not yet supported"
  }

  case object SeqAssignNotSupported extends UserError {
    override def code: String = "seqAssignNotSupported"
    override def text: String = "The `:=` statement is not yet supported"
  }

  object GeneratedPerm extends PanicBlame("Permissions for these locations should be generated.")

  case class CallableFailureNotSupported(n: Node[_]) extends UserError {
    override def code: String = "callableFailureNotSupported"
    override def text: String = n.o.messageInContext("Failures of type CallableFailure are not yet supported for this node")
  }

  case class CallableFailureNotSupportedBlame(node: Node[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = throw CallableFailureNotSupported(node)
  }
}

case class EncodeSeqProg[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {

  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()
  val currentRun: ScopedStack[SeqRun[Pre]] = ScopedStack()

  sealed trait Mode
  case object Top extends Mode
  case class InProg(prog: SeqProg[Pre]) extends Mode
  case class InRun(prog: SeqProg[Pre], run: SeqRun[Pre]) extends Mode

  def mode: Mode = (currentProg.topOption, currentRun.topOption) match {
    case (None, None) => Top
    case (Some(prog), None) => InProg(prog)
    case (Some(prog), Some(run)) => InRun(prog, run)
    case (None, Some(_)) => throw new RuntimeException()
  }

  val runSucc: mut.Map[SeqRun[Pre], Procedure[Post]] = mut.LinkedHashMap()
  val progSucc: SuccessionMap[SeqProg[Pre], Procedure[Post]] = SuccessionMap()
  val endpointSucc: SuccessionMap[(Mode, Endpoint[Pre]), Variable[Post]] = SuccessionMap()
  val variableSucc: SuccessionMap[(Mode, Variable[Pre]), Variable[Post]] = SuccessionMap()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] => currentProg.having(prog) {
      // First generate a procedure that implements the run method
      rewriteRun(prog.run)

      // Then generate a procedure that initializes all the endpoints and calls the run procedure
      // First set up the succesor variables that will be encoding the seq_program argument and endpoints
      implicit val o = prog.o
      prog.endpoints.foreach(_.drop())
      for (endpoint <- currentProg.top.endpoints) {
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
            blame = PanicBlame("TODO: endpoint constructor failure blame")
          )
        )(PanicBlame("TODO: Constructor assign failure blame"))
      }

      // Invoke the run procedure with the seq_program arguments, as well as all the endpoints
      val invokeRun = Eval(procedureInvocation[Post](
        ref = runSucc(prog.run).ref,
        args = prog.args.map(arg => Local[Post](variableSucc((mode, arg)).ref)) ++
          prog.endpoints.map(endpoint => Local[Post](endpointSucc((mode, endpoint)).ref)),
        blame = PanicBlame("TODO: run invocation failure blame")
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
      )(PanicBlame("TODO: callable failure blame")))
    }

    case _ => rewriteDefault(decl)
  }

  def rewriteRun(run: SeqRun[Pre]): Unit = {
    implicit val o: Origin = run.o.where(name = currentProg.top.o.getPreferredNameOrElse().snake + "_run")

    currentRun.having(run) {
      for (endpoint <- currentProg.top.endpoints) {
        endpointSucc((mode, endpoint)) = new Variable(TClass(succ[Class[Post]](endpoint.cls.decl)))
      }

      for (arg <- currentProg.top.args) {
        variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
      }

      runSucc(run) = globalDeclarations.declare(new Procedure(
        args = currentProg.top.args.map(arg => variableSucc((mode, arg))) ++
          currentProg.top.endpoints.map(endpoint => endpointSucc((mode, endpoint))),
        contract = dispatch(run.contract),
        body = Some(dispatch(run.body)),
        outArgs = Seq(), typeArgs = Seq(),
        returnType = TVoid(),
      )(CallableFailureNotSupportedBlame(run)))
    }
  }

  case class ForwardToAccessFailure(access: Access[_]) extends Blame[VerificationFailure] {
    override def blame(error: VerificationFailure): Unit = error match {
      case error: AssignFailed =>
        access.blame.blame(AccessInsufficientPermission(access))
      case error: InsufficientPermission =>
        access.blame.blame(AccessInsufficientPermission(access))
      case _ => ???
    }
  }

  case class ForwardToSeqAssignFailure(assign: SeqAssign[_]) extends Blame[AssignFailed] {
    override def blame(error: AssignFailed): Unit =
      assign.blame.blame(SeqAssignInsufficientPermission(assign))
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
      )(ForwardToSeqAssignFailure(assign))
    case comm @ Communicate(receiver, sender) =>
      implicit val o = comm.o
      Assign[Post](
        rewriteAccess(receiver),
        rewriteAccess(sender)
      )(ForwardToAccessFailure(receiver))
    case stat => rewriteDefault(stat)
  }

  def rewriteAccess(access: Access[Pre]): Expr[Post] =
    Deref[Post](rewriteSubject(access.subject), succ(access.field.decl))(ForwardToAccessFailure(access))(access.o)

  def rewriteSubject(subject: Subject[Pre]): Expr[Post] = subject match {
    case EndpointName(Ref(endpoint)) => Local[Post](endpointSucc((mode, endpoint)).ref)(subject.o)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = (mode, expr) match {
    case (mode, EndpointUse(Ref(endpoint))) =>
      Local[Post](endpointSucc((mode, endpoint)).ref)(expr.o)
    case (mode, Local(Ref(v))) if mode != Top && currentProg.top.args.contains(v) =>
      Local[Post](variableSucc((mode, v)).ref)(expr.o)
    case (_, expr) => rewriteDefault(expr)
  }
}
