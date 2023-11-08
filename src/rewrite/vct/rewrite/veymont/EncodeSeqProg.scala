package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{Assign, Block, Class, Communicate, Declaration, Deref, Endpoint, EndpointUse, Eval, Expr, Local, LocalDecl, Procedure, Scope, SeqAssign, SeqProg, SeqRun, Statement, TClass, TVoid, Variable}
import vct.col.origin.{DiagnosticOrigin, Origin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import EncodeSeqProg.{CommunicateNotSupported, SeqAssignNotSupported}
import vct.col.ref.Ref

import scala.collection.{mutable => mut}

object EncodeSeqProg extends RewriterBuilder {
  override def key: String = "EncodeSeqProg"
  override def desc: String = "Encodes the semantics of a parallel VeyMont program"

  case object CommunicateNotSupported extends UserError {
    override def code: String = "communicateNotSupported"
    override def text: String = "The `communicate` statement is not yet supported"
  }

  case object SeqAssignNotSupported extends UserError {
    override def code: String = "seqAssignNotSupported"
    override def text: String = "The `:=` statement is not yet supported"
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
    implicit val o: Origin = run.o.replacePrefName(currentProg.top.o.getPreferredNameOrElse() + "_run")

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
      )(PanicBlame("TODO: inner run blame")))
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case assign@SeqAssign(Ref(endpoint), Ref(field), e) =>
      implicit val o = assign.o
      Assign(
        Deref[Post](
          Local(endpointSucc((mode, endpoint)).ref),
          succ(field)
        )(PanicBlame("Private field permission management is done by VeyMont")),
        dispatch(e)
      )(PanicBlame("Private field permission management is done by VeyMont"))
    case _: Communicate[Pre] => throw CommunicateNotSupported
    case stat => rewriteDefault(stat)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = (mode, expr) match {
    case (mode, EndpointUse(Ref(endpoint))) =>
      Local[Post](endpointSucc((mode, endpoint)).ref)(expr.o)
    case (mode, Local(Ref(v))) if mode != Top && currentProg.top.args.contains(v) =>
      Local[Post](variableSucc((mode, v)).ref)(expr.o)
    case (_, expr) => rewriteDefault(expr)
  }
}
