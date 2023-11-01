package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{Assign, Block, Class, Communicate, Declaration, Endpoint, EndpointUse, Eval, Expr, Local, LocalDecl, Procedure, SeqAssign, SeqProg, SeqRun, Statement, TClass, TVoid, Variable}
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

  val runToProc: mut.Map[SeqRun[Pre], Procedure[Post]] = mut.LinkedHashMap()

  val seqProgSucc: SuccessionMap[SeqProg[Pre], Procedure[Post]] = SuccessionMap()
  val endpointSucc: SuccessionMap[(Mode, Endpoint[Pre]), Variable[Post]] = SuccessionMap()
  val variableSucc: SuccessionMap[(Mode, Variable[Pre]), Variable[Post]] = SuccessionMap()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] => currentProg.having(prog) {
      // First generate a procedure that implements the run method
      rewriteRun(prog.run)

      // Then generate a procedure that initializes all the endpoints and calls the run procedure
      // First set up the succesor variables that will be encoding the seq_program argument and endpoints
      implicit val o = DiagnosticOrigin
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

      seqProgSucc(prog) = globalDeclarations.declare(new Procedure(
        returnType = TVoid(), outArgs = Seq(), typeArgs = Seq(),
        args = prog.args.map(arg => variableSucc((mode, arg))),
        contract = dispatch(prog.contract),
        body = Some(Block(
          prog.endpoints.flatMap { endpoint =>
            val v = endpointSucc((mode, endpoint))
            Seq(
              LocalDecl(v),
              Assign(
                Local[Post](v.ref),
                procedureInvocation[Post](
                  ref = succ(endpoint.constructor.decl),
                  args = endpoint.args.map(dispatch),
                  blame = PanicBlame("TODO: endpoint constructor failure blame")
                )
              )(PanicBlame("TODO: Constructor assign failure blame"))
            )
          }
          :+
          Eval(procedureInvocation[Post](
            ref = runToProc(prog.run).ref,
            args = prog.args.map(arg => Local[Post](variableSucc((mode, arg)).ref)) ++
              prog.endpoints.map(endpoint => Local[Post](endpointSucc((mode, endpoint)).ref)),
            blame = PanicBlame("TODO: run invocation failure blame")
          ))
        ))
      )(PanicBlame("TODO: callable failure blame")))
    }
    case _ => rewriteDefault(decl)
  }

  def rewriteRun(run: SeqRun[Pre]): Unit = {
    implicit val o: Origin = DiagnosticOrigin

    currentRun.having(run) {
      for (endpoint <- currentProg.top.endpoints) {
        endpointSucc((mode, endpoint)) = new Variable(TClass(succ[Class[Post]](endpoint.cls.decl)))
      }

      for (arg <- currentProg.top.args) {
        variableSucc((mode, arg)) = new Variable(dispatch(arg.t))(arg.o)
      }

      runToProc(run) = globalDeclarations.declare(new Procedure(
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
    case _: Communicate[Pre] => throw CommunicateNotSupported
    case _: SeqAssign[Pre] => throw SeqAssignNotSupported
    case stat => rewriteDefault(stat)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = (mode, expr) match {
    case (Top, EndpointUse(Ref(endpoint))) => throw new RuntimeException()
    case (mode, EndpointUse(Ref(endpoint))) =>
      Local[Post](endpointSucc((mode, endpoint)).ref)(expr.o)
    case (mode, Local(Ref(v))) if mode != Top && currentProg.top.args.contains(v) =>
      Local[Post](variableSucc((mode, v)).ref)(expr.o)
    case (_, expr) => rewriteDefault(expr)
  }
}
