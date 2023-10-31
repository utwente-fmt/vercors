package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{AbstractRewriter, Class, Communicate, Declaration, Endpoint, Procedure, SeqProg, SeqRun, Statement, TClass, TVoid, Variable}
import vct.col.origin.{DiagnosticOrigin, Origin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import EncodeSeqProg.CommunicateNotSupported

import scala.collection.{mutable => mut}

object EncodeSeqProg extends RewriterBuilder {
  override def key: String = "EncodeSeqProg"
  override def desc: String = "Encodes the semantics of a parallel VeyMont program"

  case object CommunicateNotSupported extends UserError {
    override def code: String = "communicateNotSupported"
    override def text: String = "The `communicate` statement is not yet supported"
  }
}

case class EncodeSeqProg[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()
  val currentRun: ScopedStack[SeqRun[Pre]] = ScopedStack()

  val endpointToRunVar: mut.Map[Endpoint[Pre], Variable[Post]] = mut.LinkedHashMap()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] => currentProg.having(prog) {
      // First generate a procedure that implements the run method
      rewriteRun(prog.run)
      // Then generate a procedure that initializes all the endpoints and calls the run procedure
      generateSeqProgMain(prog)
    }
    case _ => rewriteDefault(decl)
  }

  def rewriteRun(run: SeqRun[Pre]): Unit = {
    implicit val o: Origin = DiagnosticOrigin

    for(endpoint <- currentProg.top.threads) {
      endpointToRunVar(endpoint) = new Variable(TClass(succ[Class[Post]](endpoint.cls.decl)))
    }

    currentRun.having(run) {
      globalDeclarations.declare(new Procedure(
        args = currentProg.top.threads.map(endpoint => endpointToRunVar(endpoint)),
        contract = dispatch(run.contract),
        body = Some(dispatch(run.body)),
        outArgs = Seq(), typeArgs = Seq(),
        returnType = TVoid(),
      )(PanicBlame("TODO: inner run blame"))) // CallableFailure
    }
  }

  def generateSeqProgMain(prog: SeqProg[Pre]): Unit = {
    prog.drop()
    logger.warn("SeqProg main generation not yet supported")
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case _: Communicate[Pre] => throw CommunicateNotSupported
    case stat => rewriteDefault(stat)
  }
}
