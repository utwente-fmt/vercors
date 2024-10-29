package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.rewrite.veymont.VeymontContext
import vct.rewrite.veymont.verification.EncodeChannels.ExhaleFailedToChannelInvariantNotEstablished

object EncodeChannels extends RewriterBuilder {
  override def key: String = "encodeChannels"

  override def desc: String =
    "Encodes channels using plain assignment. Encodes channel invariants using exhale/inhale."

  case class ExhaleFailedToChannelInvariantNotEstablished(comm: Communicate[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      comm.blame.blame(ChannelInvariantNotEstablished(error.failure, comm))
  }
}

case class EncodeChannels[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {
  val msgSucc = SuccessionMap[Communicate[Pre], Variable[Post]]()
  val substitutions = ScopedStack[Map[Expr[Pre], Expr[Post]]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    super.dispatch(p)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) { chor.rewriteDefault().succeed(chor) }
      case _ => super.dispatch(decl)
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case CommunicateStatement(comm) =>
        implicit val o = comm.o
        val sender = comm.sender.get.decl
        val receiver = comm.receiver.get.decl
        val m = new Variable(dispatch(comm.msg.t))(comm.o.where(name = "m"))
        msgSucc(comm) = m

        // Helper for rewriting the invariant. Regular expressions we wrap in the EndpointExpr of the sender/receiver
        // ChorExpr's we leave untouched. Those will be encoded by the EncodeStratifiedPermissions pass.
        def wrapEndpointExpr(expr: Expr[Pre], ep: Endpoint[Pre]): Expr[Post] =
          foldAny(comm.invariant.t)(unfoldStar(comm.invariant).map {
            case e: ChorExpr[Pre] => dispatch(e)
            case e => EndpointExpr(succ(ep), dispatch(e))
          })

        Scope(
          Seq(m),
          Block(Seq(
            assignLocal(
              m.get,
              EndpointExpr[Post](succ(sender), dispatch(comm.msg)),
            ),
            Exhale(currentEndpoint.having(comm.sender.get.decl) {
              wrapEndpointExpr(comm.invariant, sender)
            })(ExhaleFailedToChannelInvariantNotEstablished(comm)),
            EndpointStatement[Post](
              Some(succ(receiver)),
              Assign(dispatch(comm.target), m.get)(PanicBlame(
                "Assignment blame is handled by target expression"
              )),
            )(PanicBlame("Unused blame")),
            Inhale(currentEndpoint.having(comm.receiver.get.decl) {
              substitutions.having(Map.from(Seq(
                (Message(new DirectRef(comm)), dispatch(comm.target))
              ))) { wrapEndpointExpr(comm.invariant, receiver) }
            }),
          )),
        )
      case _ => statement.rewriteDefault()
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case e if substitutions.topOption.exists(_.contains(e)) =>
        substitutions.top(e)
      case InEndpoint(_, endpoint, Perm(loc, perm)) =>
        ChorPerm[Post](succ(endpoint), dispatch(loc), dispatch(perm))(expr.o)
      case InEndpoint(_, _, _: ChorPerm[Pre]) =>
        assert(false);
        ??? // TODO (RR): This is an error, remove when getting rid of ChorPerm as well
      case Message(Ref(comm)) => Local[Post](msgSucc.ref(comm))(comm.o)
      case Sender(Ref(comm)) =>
        EndpointName[Post](succ(comm.sender.get.decl))(expr.o)
      case Receiver(Ref(comm)) =>
        EndpointName[Post](succ(comm.receiver.get.decl))(expr.o)
      case _ => expr.rewriteDefault()
    }
}
