package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.origin.{
  Blame,
  BranchUnanimityFailed1,
  ChorStatementFailure,
  LoopUnanimityNotEstablished,
  LoopUnanimityNotMaintained,
  Origin,
  PanicBlame,
  UnsafeDontCare,
}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{RefField, RefPVLEndpoint}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstMatchHelpers.EndpointName
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.lang.LangVeyMontToCol.{
  ForwardBranchUnanimityFailed,
  ForwardLoopUnanimityFailed,
}

case object LangVeyMontToCol {
  case class ForwardBranchUnanimityFailed(branch: PVLBranch[_])
      extends Blame[ChorStatementFailure]() {
    def blame(error: ChorStatementFailure): Unit =
      error match {
        case error: BranchUnanimityFailed1 => branch.blame.blame(error)
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

case class LangVeyMontToCol[Pre <: Generation](rw: LangSpecificToCol[Pre])
    extends LazyLogging {
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

  lazy val warnedAboutAssign: Unit = {
    logger.warn(
      "Plain assignment detected in choreography. This is allowed, but technically unsound. Use `:=` instead to also check endpoint ownership."
    )
  }

  def rewriteCommunicateStatement(
      comm: PVLCommunicateStatement[Pre]
  ): CommunicateStatement[Post] = {
    val inner = comm.comm
    implicit val o = comm.o
    val newComm =
      new Communicate[Post](
        comm.inv.map(rw.dispatch).getOrElse(tt),
        receiver = Some(
          rewrite(inner.receiver.orElse(inner.inferredReceiver).getOrElse(
            throw new Exception(
              comm.o.messageInContext("Receiver unclear for this communicate")
            )
          ))
        ),
        destination = rw.dispatch(inner.target),
        sender = Some(
          rewrite(inner.sender.orElse(inner.inferredSender).getOrElse(
            throw new Exception("Sender unclear for this communicate")
          ))
        ),
        msg = rw.dispatch(inner.msg),
      )(comm.comm.blame)
    commSucc(inner) = newComm
    CommunicateStatement(newComm)(comm.o)
  }

  def rewriteRef(
      target: PVLCommTargetEndpoint[Pre]
  ): Ref[Post, Endpoint[Post]] = endpointSucc.ref(target.ref.get.decl)

  def rewrite(target: PVLCommunicateTarget[Pre]): CommunicateTarget[Post] =
    target match {
      case target: PVLCommTargetEndpoint[Pre] =>
        require(target.ref.get.decl.isSingle)
        CommTargetEndpoint(rewriteRef(target))(target.o)
      case target @ PVLCommTargetIndex(_, index) =>
        require(target.ref.get.decl.isFamily)
        CommTargetIndex[Post](
          endpointSucc.ref(target.ref.get.decl),
          rw.dispatch(index),
        )(target.o)
      case target @ PVLCommTargetRange(_, range) =>
        require(target.ref.get.decl.isFamily)
        CommTargetRange[Post](
          endpointSucc.ref(target.ref.get.decl),
          rw.dispatch(range),
        )(target.o)
    }

  def rewriteEndpoint(endpoint: PVLEndpoint[Pre]): Unit = {
    val classTypeArgs = endpoint.typeArgs.map(rw.dispatch)
    endpointSucc(endpoint) = rw.endpoints.declare(
      new Endpoint[Post](
        range = endpoint.range.map(rw.dispatch),
        cls = rw.succ[Class[Post]](endpoint.cls.decl),
        typeArgs = classTypeArgs,
        init =
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
            }.getOrElse(
              ChorRun(
                Block(Seq()),
                contract(blame =
                  UnsafeDontCare.Satisfiability("it is never unsat")
                ),
              )(PanicBlame("trivial contract"))
            ),
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
  ): Expr[Post] = {
    if (endpoint.decl.isFamily) {
      EndpointFamilyExpr[Post](endpointSucc.ref(endpoint.decl))(local.o)
    } else { EndpointName[Post](endpointSucc.ref(endpoint.decl))(local.o) }
  }

  def rewriteRun(run: PVLChorRun[Pre]): ChorRun[Post] = {
    run.drop()
    ChorRun(rw.dispatch(run.body), rw.dispatch(run.contract))(run.blame)(run.o)
  }

  def rewriteStatement(stmt: Statement[Pre]): Statement[Post] =
    stmt match {
      case stmt @ PVLEndpointStatement(endpoint, inner) =>
        EndpointStatement(
          endpoint.map(rewrite),
          currentStatement.having(inner) { rw.dispatch(inner) },
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
      case assign: Assign[Pre] =>
        warnedAboutAssign
        assign.rewriteDefault()
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
        EndpointExpr[Post](
          CommTargetEndpoint[Post](
            endpointSucc.ref(endpoint.asName.ref.get.decl)
          )(expr.o),
          Perm(rw.dispatch(loc), rw.dispatch(perm))(expr.o),
        )(expr.o)
      case expr @ PVLSender() =>
        Sender[Post](commSucc.ref(expr.ref.get.comm))(expr.o)
      case expr @ PVLReceiver() =>
        Receiver[Post](commSucc.ref(expr.ref.get.comm))(expr.o)
      case expr @ PVLMessage() =>
        Message[Post](commSucc.ref(expr.ref.get.comm))(expr.o)
      case PVLEndpointExpr(endpoint, expr) =>
        EndpointExpr(rewrite(endpoint), rw.dispatch(expr))(expr.o)
      case expr => currentExpr.having(expr) { rw.dispatch(expr) }
    }
}
