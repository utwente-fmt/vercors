package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.resolve.ctx.RefPVLEndpoint
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.rewrite.lang.LangSpecificToCol
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.lang.LangVeyMontToCol.{CommunicateNotSupported, EndpointUseNotSupported, NoRunBody, NoRunMethod}

case object LangVeyMontToCol {
  case object CommunicateNotSupported extends UserError {
    override def code: String = "communicateNotSupported"
    override def text: String = "The `communicate` statement is not yet supported"
  }

  case object EndpointUseNotSupported extends UserError {
    override def code: String = "endpointUseNotSupported"
    override def text: String = "Referencing of endpoints is not yet supported"
  }

  case class NoRunMethod(prog: PVLSeqProg[_]) extends UserError {
    override def code: String = "noRunMethod"
    override def text: String = prog.o.messageInContext(
      s"This `seq_program` has no `run` method."
    )
  }

  case class NoRunBody(prog: RunMethod[_]) extends UserError {
    override def code: String = "noRunBody"
    override def text: String = prog.o.messageInContext(
      s"This `run` method needs a body, as it is part of a `seq_program`."
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
          prog.declarations.collectFirst {
            case run: RunMethod[Pre] => rewriteRun(run)
          }.getOrElse(throw NoRunMethod(prog)),
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

  def rewriteSubject(subject: PVLCommunicateSubject[Pre]): Subject[Post] = subject match {
    case subject@PVLEndpointName(name) => EndpointName[Post](endpointSucc.ref(subject.ref.get.decl))(subject.o)
    case PVLIndexedFamilyName(family, index) => ???
    case PVLFamilyRange(family, binder, start, end) => ???
  }

  def rewriteEndpointUse(endpoint: RefPVLEndpoint[Pre], local: PVLLocal[Pre]): EndpointUse[Post] = {
    throw EndpointUseNotSupported
    // TODO: Enable when actual seq_program analysis is implemented
//    EndpointUse[Post](endpointSucc.ref(endpoint.decl))(local.o)
  }

  def rewriteRun(run: RunMethod[Pre]): SeqRun[Post] = run.body match {
    case Some(body) =>
      run.drop()
      SeqRun(rw.dispatch(body), rw.dispatch(run.contract))(run.blame)(run.o)
    case None => throw NoRunBody(run)
  }
}
