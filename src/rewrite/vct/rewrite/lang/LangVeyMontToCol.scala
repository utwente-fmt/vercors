package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.origin.{
  Blame,
  BranchUnanimityFailed,
  ChorStatementFailure,
  LoopUnanimityNotEstablished,
  LoopUnanimityNotMaintained,
  Origin,
  PanicBlame,
}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{RefField, RefPVLEndpoint}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.lang.LangVeyMontToCol.{
  AssignNotAllowed,
  ForwardBranchUnanimityFailed,
  ForwardLoopUnanimityFailed,
  NoRunMethod,
}
import vct.rewrite.veymont.InferEndpointContexts

case object LangVeyMontToCol {
  case class NoRunMethod(prog: PVLChoreography[_]) extends UserError {
    override def code: String = "noRunMethod"
    override def text: String =
      prog.o.messageInContext(s"This choreography has no `run` method.")
  }

  case class AssignNotAllowed(assign: Assign[_]) extends UserError {
    override def code: String = "assignNotAllowed"
    override def text: String =
      assign.o.messageInContext(
        "Plain assignment is not allowed in a choreography. Use `:=` instead."
      )
  }

  case class ForwardBranchUnanimityFailed(branch: PVLBranch[_])
      extends Blame[ChorStatementFailure]() {
    def blame(error: ChorStatementFailure): Unit =
      error match {
        case error: BranchUnanimityFailed => branch.blame.blame(error)
        case error =>
          PanicBlame(
            s"ChorStatement got error ${error.code}, but it only supports branch unanimity"
          ).blame(error)
      }
  }

  case class ForwardLoopUnanimityFailed(loop: PVLLoop[_])
      extends Blame[ChorStatementFailure]() {
    def blame(error: ChorStatementFailure): Unit =
      error match {
        case error: LoopUnanimityNotMaintained => loop.blame.blame(error)
        case error: LoopUnanimityNotEstablished => loop.blame.blame(error)
        case error =>
          PanicBlame(
            s"ChorStatement got error ${error.code}, but it only supports loop unanimity maintained/established"
          ).blame(error)
      }
  }
}

case class LangVeyMontToCol[Pre <: Generation](
    rw: LangSpecificToCol[Pre],
    allowAssign: Boolean = false,
) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val chorSucc: SuccessionMap[PVLChoreography[Pre], Choreography[Post]] =
    SuccessionMap()
  val endpointSucc: SuccessionMap[PVLEndpoint[Pre], Endpoint[Post]] =
    SuccessionMap()
  val commSucc: SuccessionMap[PVLCommunicate[Pre], Communicate[Post]] =
    SuccessionMap()

  val currentProg: ScopedStack[PVLChoreography[Pre]] = ScopedStack()
  val currentStatement: ScopedStack[Statement[Pre]] = ScopedStack()
  val currentExpr: ScopedStack[Expr[Pre]] = ScopedStack()

  def rewriteCommunicateStatement(
      comm: PVLCommunicateStatement[Pre]
  ): CommunicateStatement[Post] = {
    val inner = comm.comm
    val newComm =
      new Communicate[Post](
        rw.dispatch(comm.inv.getOrElse(tt[Pre])),
        Some(endpointSucc.ref(inner.inferredReceiver.get)),
        rw.dispatch(inner.target),
        Some(endpointSucc.ref(inner.inferredSender.get)),
        rw.dispatch(inner.msg),
      )(inner.blame)(comm.o)
    commSucc(inner) = newComm
    CommunicateStatement(newComm)(comm.o)
  }

  def rewriteEndpointName(
      name: PVLEndpointName[Pre]
  ): Ref[Post, Endpoint[Post]] = endpointSucc.ref(name.ref.get.decl)

  def rewriteEndpoint(endpoint: PVLEndpoint[Pre]): Unit = {
    // TODO (RR): Sort out blame. See EncodeChoreography for old blame
    val classTypeArgs = endpoint.typeArgs.map(rw.dispatch)
    endpointSucc(endpoint) = rw.endpoints.declare(
      new Endpoint[Post](
        rw.succ[Class[Post]](endpoint.cls.decl),
        classTypeArgs,
        ConstructorInvocation(
          rw.pvl.constructorSucc(endpoint.ref.get),
          classTypeArgs,
          endpoint.args.map(rw.dispatch),
          Seq(),
          Seq(),
          Seq(),
          Seq(),
        )(endpoint.blame)(endpoint.o),
      )(endpoint.o)
    )
  }

  def rewriteChoreography(prog: PVLChoreography[Pre]): Unit = {
    implicit val o: Origin = prog.o
    rw.currentThis.having(ThisChoreography[Post](chorSucc.ref(prog))) {
      currentProg.having(prog) {
        chorSucc(prog) = rw.globalDeclarations.declare(
          new Choreography(
            rw.dispatch(prog.contract),
            rw.variables.collect(prog.args.map(rw.dispatch(_)))._1,
            rw.endpoints.collect(prog.declarations.foreach {
              case endpoint: PVLEndpoint[Pre] => rewriteEndpoint(endpoint)
              case _ =>
            })._1,
            None,
            prog.declarations.collectFirst { case run: PVLChorRun[Pre] =>
              rewriteRun(run)
            }.getOrElse(throw NoRunMethod(prog)),
            rw.classDeclarations.collect(prog.declarations.foreach {
              case _: PVLChorRun[Pre] =>
              case _: PVLEndpoint[Pre] =>
              case decl => rw.dispatch(decl)
            })._1,
          )(prog.blame)(prog.o)
        )
      }
    }
  }

  def rewriteEndpointUse(
      endpoint: RefPVLEndpoint[Pre],
      local: PVLLocal[Pre],
  ): EndpointName[Post] =
    EndpointName[Post](endpointSucc.ref(endpoint.decl))(local.o)

  def rewriteRun(run: PVLChorRun[Pre]): ChorRun[Post] = {
    run.drop()
    ChorRun(rw.dispatch(run.body), rw.dispatch(run.contract))(run.blame)(run.o)
  }

  def rewriteStatement(stmt: Statement[Pre]): Statement[Post] =
    stmt match {
      case stmt @ PVLEndpointStatement(endpointName, inner) =>
        EndpointStatement[Post](
          endpointName.map(rewriteEndpointName),
          inner.rewriteDefault(),
        )(stmt.blame)(stmt.o)
      case eval: Eval[Pre] =>
        EndpointStatement[Post](None, eval.rewriteDefault())(PanicBlame(
          "Inner statement cannot fail"
        ))(stmt.o)
      case _: Block[Pre] | _: Scope[Pre] =>
        currentStatement.having(stmt) { rw.dispatch(stmt) }
      case branch: PVLBranch[Pre] =>
        ChorStatement(currentStatement.having(stmt) { rw.dispatch(stmt) })(
          ForwardBranchUnanimityFailed(branch)
        )(stmt.o)
      case loop: PVLLoop[Pre] =>
        ChorStatement(currentStatement.having(stmt) { rw.dispatch(stmt) })(
          ForwardLoopUnanimityFailed(loop)
        )(stmt.o)
      case comm: PVLCommunicateStatement[Pre] =>
        rewriteCommunicateStatement(comm)
      // Any statement not listed here, we put in ChorStatement. ChorStatementImpl defines which leftover statement we tolerate in choreographies
      case stmt =>
        currentStatement.having(stmt) {
          ChorStatement(rw.dispatch(stmt))(PanicBlame(
            "The internal statement cannot cause an  error on this ChorStatement"
          ))(stmt.o)
        }
    }

  def rewriteExpr(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case PVLChorPerm(endpoint, loc, perm) =>
        ChorPerm[Post](
          rewriteEndpointName(endpoint),
          rw.dispatch(loc),
          rw.dispatch(perm),
        )(expr.o)
      case expr @ PVLSender() =>
        Sender[Post](commSucc.ref(expr.ref.get.comm))(expr.o)
      case expr @ PVLReceiver() =>
        Receiver[Post](commSucc.ref(expr.ref.get.comm))(expr.o)
      case expr @ PVLMessage() =>
        Message[Post](commSucc.ref(expr.ref.get.comm))(expr.o)
      case PVLEndpointExpr(endpoint, expr) =>
        EndpointExpr(rewriteEndpointName(endpoint), rw.dispatch(expr))(expr.o)
      case expr => currentExpr.having(expr) { rw.dispatch(expr) }
    }
}
