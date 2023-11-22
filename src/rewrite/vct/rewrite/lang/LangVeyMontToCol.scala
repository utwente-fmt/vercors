package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.resolve.ctx.RefPVLEndpoint
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.lang.LangVeyMontToCol.NoRunMethod

case object LangVeyMontToCol {
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
}

case class LangVeyMontToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val seqProgSucc: SuccessionMap[PVLSeqProg[Pre], SeqProg[Post]] = SuccessionMap()
  val endpointSucc: SuccessionMap[PVLEndpoint[Pre], Endpoint[Post]] = SuccessionMap()

  val currentProg: ScopedStack[PVLSeqProg[Pre]] = ScopedStack()

  def rewriteCommunicate(comm: PVLCommunicate[Pre]): Communicate[Post] =
    Communicate(rewriteAccess(comm.receiver), rewriteAccess(comm.sender))(comm.o)

  def rewriteAccess(access: PVLAccess[Pre]): Access[Post] =
    Access[Post](rewriteSubject(access.subject), rw.succ(access.ref.get.decl))(access.blame)(access.o)

  def rewriteSubject(subject: PVLSubject[Pre]): Subject[Post] = subject match {
    case subject@PVLEndpointName(name) => EndpointName[Post](endpointSucc.ref(subject.ref.get.decl))(subject.o)
    case PVLIndexedFamilyName(family, index) => ???
    case PVLFamilyRange(family, binder, start, end) => ???
  }

  def rewriteEndpoint(endpoint: PVLEndpoint[Pre]): Unit =
    endpointSucc(endpoint) = rw.endpoints.declare(new Endpoint(
      rw.succ[Class[Post]](endpoint.cls.decl),
      rw.pvl.constructorSucc(endpoint.ref.get),
      endpoint.args.map(rw.dispatch)
    )(endpoint.o))

  def rewriteSeqProg(prog: PVLSeqProg[Pre]): Unit = {
    implicit val o: Origin = prog.o
    rw.currentThis.having(ThisSeqProg[Post](seqProgSucc.ref(prog))) {
      currentProg.having(prog) {
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
              case run: PVLSeqRun[Pre] => rewriteRun(run)
            }.getOrElse(throw NoRunMethod(prog)),
            rw.classDeclarations.collect(
              prog.declarations.foreach {
                case _: PVLSeqRun[Pre] =>
                case _: PVLEndpoint[Pre] =>
                case decl => rw.dispatch(decl)
              }
            )._1
          )(prog.o)
        )
      }
    }
  }

  def rewriteEndpointUse(endpoint: RefPVLEndpoint[Pre], local: PVLLocal[Pre]): EndpointUse[Post] =
    EndpointUse[Post](endpointSucc.ref(endpoint.decl))(local.o)

  def rewriteRun(run: PVLSeqRun[Pre]): SeqRun[Post]  = {
      run.drop()
      SeqRun(rw.dispatch(run.body), rw.dispatch(run.contract))(run.blame)(run.o)
  }

  def rewriteSeqAssign(assign: PVLSeqAssign[Pre]): SeqAssign[Post] =
    SeqAssign[Post](endpointSucc.ref(assign.receiver.decl), rw.succ(assign.field.decl), rw.dispatch(assign.value))(assign.blame)(assign.o)

  def rewriteBranch(branch: PVLBranch[Pre]): UnresolvedSeqBranch[Post] =
    UnresolvedSeqBranch(branch.branches.map { case (e, s) => (rw.dispatch(e), rw.dispatch(s)) })(branch.blame)(branch.o)

  def rewriteLoop(loop: PVLLoop[Pre]): UnresolvedSeqLoop[Post] =
    UnresolvedSeqLoop(rw.dispatch(loop.cond), rw.dispatch(loop.contract), rw.dispatch(loop.body))(loop.blame)(loop.o)

}
