package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.origin.{Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{RefField, RefPVLEndpoint}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.lang.LangVeyMontToCol.{AssignNotAllowed, NoRunMethod}
import vct.rewrite.veymont.InferEndpointContexts

case object LangVeyMontToCol {
  case class NoRunMethod(prog: PVLChoreography[_]) extends UserError {
    override def code: String = "noRunMethod"
    override def text: String = prog.o.messageInContext(
      s"This `seq_program` has no `run` method."
    )
  }

  case class AssignNotAllowed(assign: Assign[_]) extends UserError {
    override def code: String = "assignNotAllowed"
    override def text: String = assign.o.messageInContext(
      "Plain assignment is not allowed in `seq_program`. Use `:=` instead."
    )
  }
}

case class LangVeyMontToCol[Pre <: Generation](rw: LangSpecificToCol[Pre], allowAssign: Boolean = false) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val chorSucc: SuccessionMap[PVLChoreography[Pre], Choreography[Post]] = SuccessionMap()
  val endpointSucc: SuccessionMap[PVLEndpoint[Pre], Endpoint[Post]] = SuccessionMap()
  val commSucc: SuccessionMap[PVLCommunicate[Pre], Communicate[Post]] = SuccessionMap()

  val currentProg: ScopedStack[PVLChoreography[Pre]] = ScopedStack()
  val currentStatement: ScopedStack[Statement[Pre]] = ScopedStack()
  val currentExpr: ScopedStack[Expr[Pre]] = ScopedStack()

  def rewriteCommunicate(comm: PVLCommunicate[Pre]): CommunicateStatement[Post] = {
    val newComm = new Communicate[Post](
        comm.invariant.map(rw.dispatch).getOrElse(tt),
        comm.receiver.map(rewriteEndpointName).orElse(Some(rw.succ[Endpoint[Post]](InferEndpointContexts.getEndpoint(comm.target)))),
        rw.dispatch(comm.target),
        comm.sender.map(rewriteEndpointName).orElse(Some(rw.succ[Endpoint[Post]](InferEndpointContexts.getEndpoint(comm.target)))),
        rw.dispatch(comm.msg))(comm.blame)(comm.o)
    commSucc(comm) = newComm
    CommunicateStatement(newComm)(comm.o)
  }

  def rewriteEndpointName(name: PVLEndpointName[Pre]): Ref[Post, Endpoint[Post]] =
    endpointSucc.ref(name.ref.get.decl)

  def rewriteEndpoint(endpoint: PVLEndpoint[Pre]): Unit =
    endpointSucc(endpoint) = rw.endpoints.declare(new Endpoint(
      rw.succ[Class[Post]](endpoint.cls.decl),
      endpoint.typeArgs.map(rw.dispatch),
      rw.pvl.constructorSucc(endpoint.ref.get),
      endpoint.args.map(rw.dispatch)
    )(endpoint.blame)(endpoint.o))

  def rewriteChoreography(prog: PVLChoreography[Pre]): Unit = {
    implicit val o: Origin = prog.o
    rw.currentThis.having(ThisChoreography[Post](chorSucc.ref(prog))) {
      currentProg.having(prog) {
        chorSucc(prog) = rw.globalDeclarations.declare(
          new Choreography(
            rw.dispatch(prog.contract),
            rw.variables.collect(prog.args.map(rw.dispatch(_)))._1,
            rw.endpoints.collect(
              prog.declarations.foreach {
                case endpoint: PVLEndpoint[Pre] => rewriteEndpoint(endpoint)
                case _ =>
              },
            )._1,
            None,
            prog.declarations.collectFirst {
              case run: PVLChorRun[Pre] => rewriteRun(run)
            }.getOrElse(throw NoRunMethod(prog)),
            rw.classDeclarations.collect(
              prog.declarations.foreach {
                case _: PVLChorRun[Pre] =>
                case _: PVLEndpoint[Pre] =>
                case decl => rw.dispatch(decl)
              }
            )._1
          )(prog.blame)(prog.o)
        )
      }
    }
  }

  def rewriteEndpointUse(endpoint: RefPVLEndpoint[Pre], local: PVLLocal[Pre]): EndpointName[Post] =
    EndpointName[Post](endpointSucc.ref(endpoint.decl))(local.o)

  def rewriteRun(run: PVLChorRun[Pre]): ChorRun[Post]  = {
      run.drop()
      ChorRun(rw.dispatch(run.body), rw.dispatch(run.contract))(run.blame)(run.o)
  }

  def rewriteBranch(branch: PVLBranch[Pre]): UnresolvedChorBranch[Post] =
    UnresolvedChorBranch(branch.branches.map { case (e, s) => (rw.dispatch(e), rw.dispatch(s)) })(branch.blame)(branch.o)

  def rewriteLoop(loop: PVLLoop[Pre]): UnresolvedChorLoop[Post] =
    UnresolvedChorLoop(rw.dispatch(loop.cond), rw.dispatch(loop.contract), rw.dispatch(loop.body))(loop.blame)(loop.o)

  def rewriteStatement(stmt: Statement[Pre]): Statement[Post] = stmt match {
    case stmt @ PVLChorStatement(endpointName, inner) =>
      ChorStatement[Post](
        endpointName.map(rewriteEndpointName),
        inner.rewriteDefault()
      )(stmt.blame)(stmt.o)
    case _: Block[Pre] | _: Scope[Pre] => currentStatement.having(stmt) { rw.dispatch(stmt) }
    case branch: PVLBranch[Pre] => rewriteBranch(branch)
    case loop: PVLLoop[Pre] => rewriteLoop(loop)
    case comm: PVLCommunicate[Pre] => rewriteCommunicate(comm)
    case stmt => currentStatement.having(stmt) {
      ChorStatement(None, rw.dispatch(stmt))(PanicBlame("Arbitratry statement blame missing"))(stmt.o)
    }
  }

  def rewriteExpr(expr: Expr[Pre]): Expr[Post] = expr match {
    case PVLChorPerm(endpoint, loc, perm) =>
      ChorPerm[Post](rewriteEndpointName(endpoint), rw.dispatch(loc), rw.dispatch(perm))(expr.o)
    case expr @ PVLSender() => Sender[Post](commSucc.ref(expr.ref.get))(expr.o)
    case expr @ PVLReceiver() => Receiver[Post](commSucc.ref(expr.ref.get))(expr.o)
    case expr @ PVLMessage() => Message[Post](commSucc.ref(expr.ref.get))(expr.o)
    case expr => currentExpr.having(expr) { rw.dispatch(expr) }
  }
}
