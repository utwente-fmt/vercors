package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.AstMatchHelpers.EndpointName
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

  def singleMessageExchange(
      comm: Communicate[Pre],
      sender: CommunicateTarget[Post],
      receiver: CommunicateTarget[Post],
  ): Statement[Post] = {
    implicit val o = comm.o
    val m = new Variable(dispatch(comm.msg.t))(comm.o.where(name = "m"))
    msgSucc(comm) = m

    // Helper for rewriting the invariant. Regular expressions we wrap in the EndpointExpr of the sender/receiver
    // ChorExpr's we leave untouched. Those will be encoded by the EncodeStratifiedPermissions pass.
    def wrapEndpointExpr(
        expr: Expr[Pre],
        ct: CommunicateTarget[Post],
    ): Expr[Post] =
      foldAny(expr.t)(unfoldStar(expr).map {
        case e: ChorExpr[Pre] => dispatch(e)
        case e => EndpointExpr(ct, dispatch(e))
      }).getOrElse(tt)

    Scope(
      Seq(m),
      Block(Seq(
        assignLocal(m.get, EndpointExpr[Post](sender, dispatch(comm.msg))),
        Exhale(wrapEndpointExpr(comm.invariant, sender))(
          ExhaleFailedToChannelInvariantNotEstablished(comm)
        ),
        EndpointStatement[Post](
          Some(receiver),
          Assign(dispatch(comm.destination), m.get)(PanicBlame(
            "Assignment blame is handled by target expression"
          )),
        )(PanicBlame("Unused blame")),
        Inhale(
          substitutions.having(Map.from(Seq(
            (Message(new DirectRef(comm)), dispatch(comm.destination))
          ))) { wrapEndpointExpr(comm.invariant, receiver) }
        ),
      )),
    )
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case CommunicateStatement(comm)
          if comm.sender.get.isSingle && comm.receiver.get.isSingle =>
        singleMessageExchange(
          comm,
          dispatch(comm.sender.get),
          dispatch(comm.receiver.get),
        )

      // TODO (RR): Delete this when merging
      // Saving this code just in case I need it later
//        implicit val o = comm.o
//        val sender = comm.sender.get.asName.endpoint
//        val receiver = comm.receiver.get.asName.endpoint
//        val m = new Variable(dispatch(comm.msg.t))(comm.o.where(name = "m"))
//        msgSucc(comm) = m
//
//        // Helper for rewriting the invariant. Regular expressions we wrap in the EndpointExpr of the sender/receiver
//        // ChorExpr's we leave untouched. Those will be encoded by the EncodeStratifiedPermissions pass.
//        def wrapEndpointExpr(expr: Expr[Pre], ep: Endpoint[Pre]): Expr[Post] =
//          foldAny1(expr.t)(unfoldStar(expr).map {
//            case e: ChorExpr[Pre] => dispatch(e)
//            case e => EndpointExpr(CommTargetEndpoint(succ(ep)), dispatch(e))
//          })
//
//        Scope(
//          Seq(m),
//          Block(Seq(
//            assignLocal(
//              m.get,
//              EndpointExpr[Post](
//                CommTargetEndpoint(succ(sender)),
//                dispatch(comm.msg),
//              ),
//            ),
//            Exhale(currentEndpoint.having(sender) {
//              wrapEndpointExpr(comm.invariant, sender)
//            })(ExhaleFailedToChannelInvariantNotEstablished(comm)),
//            EndpointStatement[Post](
//              Some(CommTargetEndpoint(succ(receiver))),
//              Assign(dispatch(comm.destination), m.get)(PanicBlame(
//                "Assignment blame is handled by target expression"
//              )),
//            )(PanicBlame("Unused blame")),
//            Inhale(currentEndpoint.having(comm.receiver.get.asName.endpoint) {
//              substitutions.having(Map.from(Seq(
//                (Message(new DirectRef(comm)), dispatch(comm.destination))
//              ))) { wrapEndpointExpr(comm.invariant, receiver) }
//            }),
//          )),
//        )

      case CommunicateStatement(comm)
          if comm.sender.get.isRange || comm.receiver.get.isRange =>
        assert(comm.sender.get.isRange)
        implicit val o = comm.o

        val CommTargetRange(Ref(sender), RangeBinder(v, low, high)) =
          comm.sender.get
        val CommTargetIndex(Ref(receiver), i) = comm.receiver.get

        variables.scope {
          val parV = variables.succeedOnly(v, v.rewriteDefault())

          val block =
            ParBlock(
              new ParBlockDecl()(o.where(name = "c")),
              Seq(IterVariable(parV, dispatch(low), dispatch(high))),
              tt,
              tt,
              tt,
              singleMessageExchange(
                comm,
                CommTargetIndex[Post](succ(sender), Local(parV.ref)),
                CommTargetIndex[Post](succ(receiver), dispatch(i)),
              ),
            )(PanicBlame("Unexpected error from par block encoding a comm!"))

          ParStatement(block)
        }

      case _ => statement.rewriteDefault()
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case e if substitutions.topOption.exists(_.contains(e)) =>
        substitutions.top(e)
      case Message(Ref(comm)) => Local[Post](msgSucc.ref(comm))(comm.o)
      case Sender(Ref(comm)) =>
        EndpointName[Post](succ(comm.sender.get.asName.endpoint))(expr.o)
      case Receiver(Ref(comm)) =>
        EndpointName[Post](succ(comm.receiver.get.asName.endpoint))(expr.o)
      case _ => expr.rewriteDefault()
    }
}
