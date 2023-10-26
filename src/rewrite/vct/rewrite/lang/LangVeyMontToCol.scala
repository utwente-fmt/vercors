package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.rewrite.lang.LangSpecificToCol
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.lang.LangVeyMontToCol.{CommunicateNotSupported, NoRunMethod}

case object LangVeyMontToCol {
  case object CommunicateNotSupported extends UserError {
    override def code: String = "communicateNotSupported"
    override def text: String = "The `communicate` statement is not yet supported"
  }

  case class NoRunMethod(prog: PVLSeqProg[_]) extends UserError {
    override def code: String = "noRunMethod"
    override def text: String = prog.o.messageInContext(
      s"This `seq_program` has no `run` method."
    )
  }
}

case class LangVeyMontToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val seqProgSucc: SuccessionMap[PVLSeqProg[Pre], SeqProg[Post]] = SuccessionMap()
  val endpointSucc: SuccessionMap[PVLEndpoint[Pre], Endpoint[Post]] = SuccessionMap()

  def rewriteCommunicate(comm: PVLCommunicate[Pre]): Communicate[Post] = {
    throw CommunicateNotSupported
  }

  def rewriteEndpoint(endpoint: PVLEndpoint[Pre]): Unit =
    endpointSucc(endpoint) = rw.endpoints.declare(new Endpoint(
      rw.succ[Class[Post]](endpoint.cls.decl),
      endpoint.args.map(rw.dispatch)
    )(endpoint.o))

  def rewriteSeqProg(prog: PVLSeqProg[Pre]): Unit = {
    implicit val o: Origin = prog.o
    rw.currentThis.having(ThisSeqProg[Post](seqProgSucc.ref(prog))) {
      seqProgSucc(prog) = rw.globalDeclarations.declare(
        new SeqProg(
          rw.dispatch(prog.contract),
          rw.variables.collect(prog.args.map(rw.dispatch(_)))._1,
          rw.endpoints.collect(
            prog.declarations.foreach {
              case endpoint: PVLEndpoint[Pre] => rewriteEndpoint(endpoint)
              case _ =>
            },
          )._1,
          rw.classDeclarations.collect(
            prog.declarations.foreach {
              case run: RunMethod[Pre] => rw.classDeclarations.succeed(run, run.rewrite())
              case _ =>
            }
          )._1.headOption.getOrElse(throw NoRunMethod(prog)),
          rw.classDeclarations.collect(
            prog.declarations.foreach {
              case _: RunMethod[Pre] =>
              case _: PVLEndpoint[Pre] =>
              case decl => rw.dispatch(decl)
            }
          )._1
        )(prog.o)
      )
    }
  }

//  def rewriteEndpointName
}
