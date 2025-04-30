package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.AstMatchHelpers.EndpointName
import vct.col.util.{AstBuildHelpers, SuccessionMap}
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

  case class ChannelInvInliner(
      message: Expr[Post],
      sender: Expr[Post],
      receiver: Expr[Post],
  ) extends Rewriter[Pre] {
    override val allScopes: AllScopes[Pre, Post] = EncodeChannels.this.allScopes

    override def dispatch(expr: Expr[Pre]): Expr[Post] =
      expr match {
        case Message(_) => message
        case Sender(_) => sender
        case Receiver(_) => receiver
        case _ => expr.rewriteDefault()
      }
  }

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
        case e => EndpointExpr(ct, Seq(), dispatch(e))
      }).getOrElse(tt)

    Scope(
      Seq(m),
      Block(Seq(
        assignLocal(
          m.get,
          EndpointExpr[Post](sender, Seq(), dispatch(comm.msg)),
        ),
        // TODO: Need to set the proper replacements for sender, receiver, msg here
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
        // For now we only support the sender defining the range
        assert(comm.sender.get.isRange)
        implicit val o = comm.o

        // Check in a hacky way that the msg/dst is also a field assignment
        val Deref(_, Ref(srcField)) = comm.msg
        val Deref(_, Ref(dstField)) = comm.destination
        val CommTargetRange(Ref(sender), RangeBinder(v, low, high)) =
          comm.sender.get
        val CommTargetIndex(Ref(receiver), i) = comm.receiver.get

        /*
        - define new p in ADT of type senderClass -> TRational
        - assume ALL i : low .. high; 0 < p(sender[i]) && p(sender[i]) < CurPerm(sender[i].f)
        - add to requires, ensures of par block
        - add inv as endpoint expr to requires, ensures
        - replace sender, receiver, msg appropriately
         */

        val eps =
          function(
            args = Seq(new Variable(dispatch(sender.singleType))),
            returnType = TRational(),
            blame = PanicBlame("TODO"),
            contractBlame = PanicBlame("TODO"),
          )(comm.o.where(name = "eps"))
        globalDeclarations.declare(eps)

        val epsSpec = Assume[Post](forrange[Post](
          dispatch(low),
          dispatch(high),
          (i: Local[Post]) => {
            val target = CommTargetIndex[Post](succ(sender), i)
            val obj = CtExpr(target)
            val frac = functionInvocation[Post](
              ref = eps.ref,
              args = Seq(obj),
              blame = PanicBlame("TODO"),
            )
            (const(0) < frac) &&
            (frac < EndpointExpr(
              target,
              Seq(),
              CurPerm(FieldLocation[Post](obj, succ(srcField))),
            ))
          },
        ))

        variables.scope {
          val parV = variables.succeedOnly(v, v.rewriteDefault())

          val senderTarget = CommTargetIndex[Post](
            succ(sender),
            Local(parV.ref),
          )
          val senderExpr = CtExpr(senderTarget)
          val receiverTarget = CommTargetIndex[Post](
            succ(receiver),
            dispatch(i),
          )
          val receiverExpr = CtExpr(receiverTarget)
          val msgPerm: Expr[Post] = EndpointExpr(
            senderTarget,
            Seq(),
            Perm(
              FieldLocation(senderExpr, succ(srcField)),
              functionInvocation[Post](
                ref = eps.ref,
                args = Seq(senderExpr),
                blame = PanicBlame("TODO"),
              ),
            ),
          )

          val dstPerm: Expr[Post] = EndpointExpr(
            receiverTarget,
            Seq(),
            Perm(FieldLocation(receiverExpr, succ(dstField)), WritePerm()),
          )

          // TODO (RR): Check injectivity!

          // First the exhale
          val exhale =
            Exhale(foldStar(unfoldStar(comm.invariant).map { e =>
              quantifyInvPart(
                comm.ref,
                succ(sender),
                succ(srcField),
                succ(receiver),
                succ(dstField),
                Sender(comm.ref),
                low,
                high,
                v,
                i,
                e,
              )(comm.o)
            }))(PanicBlame("TODO: Forward exhale failed"))
          val inhale = Inhale(foldStar(unfoldStar(comm.invariant).map { e =>
            quantifyInvPart(
              comm.ref,
              succ(sender),
              succ(srcField),
              succ(receiver),
              succ(dstField),
              Receiver(comm.ref),
              low,
              high,
              v,
              i,
              e,
            )(comm.o)
          }))

//          val block =
//            ParBlock(
//              new ParBlockDecl()(o.where(name = "c")),
//              Seq(IterVariable(parV, dispatch(low), dispatch(high))),
//              tt,
//              msgPerm &* dstPerm &* EndpointExpr(
//                senderTarget,
//                Seq(),
//                ChannelInvInliner(dispatch(comm.msg), senderExpr, receiverExpr)
//                  .dispatch(comm.invariant),
//              ),
//              msgPerm &* dstPerm &* EndpointExpr(
//                receiverTarget,
//                Seq(),
//                ChannelInvInliner(
//                  dispatch(comm.destination),
//                  senderExpr,
//                  receiverExpr,
//                ).dispatch(comm.invariant),
//              ),
//              // TODO (RR): Is there a reason we want to emit the implementation of the par block? Can't think of one, except checking for mistakes in the encoding...?
//              // singleMessageExchange(comm, senderTarget, receiverTarget),
//              Assume(ff),
//            )(PanicBlame("Unexpected error from par block encoding a comm!"))

//          Block(Seq(epsSpec, ParStatement(block)))
          Block(Seq(epsSpec, exhale, inhale))
        }

      case _ => statement.rewriteDefault()
    }

  case class PureRewriter(substitutions: Map[Expr[Pre], Expr[Post]])
      extends Rewriter[Pre] {
    override val allScopes: AllScopes[Pre, Post] = EncodeChannels.this.allScopes

    override def dispatch(expr: Expr[Pre]): Expr[Post] = {
      substitutions.get(expr) match {
        case Some(value) => value
        case None => expr.rewriteDefault()
      }
    }
  }

  def pureRewrite(
      e: Expr[Pre],
      substitutions: (Expr[Pre], Expr[Post])*
  ): Expr[Post] = PureRewriter(substitutions.toMap).dispatch(e)

  // Encodes a part of a channel invariant "e", such that it only receives binders for the indices of the sender/receiver
  // when these binders are actually used in "e". E.g. if the channel inv contains "\msg == 3", then when exhaling the
  // invariant this could be encoded as "∀int i = low .. high; F[i].f == 3". Note how only the sending range is included,
  // not the receiving range. Another example: "\sender.x == \msg" has to be encoded when inhaling (= receiving) as:
  // ∀int i := low .. high, j := i + 1; F[i].x == G[j].g
  // where "i + 1" is the destination index expression from the communicate statement.
  // TODO (RR): Change this to take primaryParty (\sender or \receiver) and dependentParty (the other). This will probably require that we actually define the inverse function. This is then used to compute the index of primaryParty when the context is dependentParty at the inhale site.
  def quantifyInvPart(
      comm: Ref[Pre, Communicate[
        Pre
      ]], // The communicate statement where e appears in the channel inv
      F: Ref[Post, Endpoint[Post]], // Sender family
      f: Ref[Post, InstanceField[Post]], // Sender field
      G: Ref[Post, Endpoint[Post]], // Receiving family
      g: Ref[Post, InstanceField[Post]], // Receiver field
      ctx: ChannelInvPrimitive[Pre],
      // low..high range of the sender
      low: Expr[Pre],
      high: Expr[Pre],
      // Destination index expression and index variable of the range
      v: Variable[Pre],
      destinationIdx: Expr[Pre],
      // Invariant part to be encoded
      e: Expr[Pre],
  )(implicit o: Origin): Expr[Post] = {
    def rangeOf(ctx: ChannelInvPrimitive[_]) = {
      ctx match {
        case Sender(_) => (dispatch(low), dispatch(high))
        case Receiver(_) =>
          (
            pureRewrite(destinationIdx, v.get -> dispatch(low)),
            pureRewrite(destinationIdx, v.get -> dispatch(high)),
          )
      }
    }

    // Include sender/receiver either because of the context or because the respective keyword is present
    val ctxIsSender = ctx match { case Sender(_) => true; case _ => false }
    val includeSender = ctxIsSender || e.exists { case Sender(_) => true }
    val ctxIsReceiver = ctx match { case Receiver(_) => true; case _ => false }
    val includeReceiver = ctxIsReceiver || e.exists { case Receiver(_) => true }

    //// Binders to occur in the wrapping quantifier
    val senderIdx = new Variable[Post](TInt())(o.where(name = "i"))
    val receiverIdx = new Variable[Post](TInt())(o.where(name = "j"))

    //// Construct message based on the context
    val sender = CtExpr(CommTargetIndex(F, senderIdx.get))
    val receiver = CtExpr(CommTargetIndex(G, receiverIdx.get))
    val msg =
      ctx match {
        case Sender(_) =>
          Deref(sender, f)(PanicBlame("TODO: Forward blame properly"))
        case Receiver(_) =>
          Deref(receiver, g)(PanicBlame("TODO: Forward blame properly"))
      }

    val newE = pureRewrite(
      e,
      Sender(comm) -> sender,
      Receiver(comm) -> receiver,
      Message(comm) -> msg,
    )

    //// If there is both the sender and receiver, the ranges are i := low .. high, j := destinationIdx(v -> i)
    //// If it's just the sender, i := low .. high
    //// If it's just the receiver, j := d(low) .. d(high)
    (includeSender, includeReceiver) match {
      case (true, true) =>
        // (\endpoint F[senderIdx := low' .. high'], ∀int j = desinationIdx(v -> i); e')
        EndpointExpr(
          CommTargetRange(
            F,
            RangeBinder(senderIdx, dispatch(low), dispatch(high)),
          ),
          Seq(receiverIdx),
          (receiverIdx.get ===
            pureRewrite(destinationIdx, v.get -> senderIdx.get)) ==> newE,
        )(e.o)
      case (true, false) =>
        // (\endpoint F[senderIdx := low' .. high']; e')
        EndpointExpr(
          CommTargetRange(
            F,
            RangeBinder(senderIdx, dispatch(low), dispatch(high)),
          ),
          Seq(),
          newE,
        )(e.o)
      case (false, true) =>
        // (\endpoint G[receiverIdx := destinationIdx(v -> low) .. destinationIdx(v -> high); e')
        val (newLow, newHigh) = rangeOf(Receiver(comm))
        EndpointExpr(
          CommTargetRange(
            G,
            RangeBinder(receiverIdx, dispatch(low), dispatch(high)),
          ),
          Seq(),
          newE,
        )(e.o)
      case (false, false) => ??? // Cannot happen
    }
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case e if substitutions.topOption.exists(_.contains(e)) =>
        substitutions.top(e)
      case Message(Ref(comm)) => Local[Post](msgSucc.ref(comm))(comm.o)
      case Sender(Ref(comm)) => CtExpr(dispatch(comm.sender.get))(expr.o)
      // TODO (RR): The sender/receiver case only work in the case of singular endpoints.
      //            The par case is avoided by using ChannelInvInliner somewhere else.
      //            I should probably refactor these two approaches into one.
//        EndpointName[Post](succ(comm.sender.get.asName.endpoint))(expr.o)
      case Receiver(Ref(comm)) => CtExpr(dispatch(comm.receiver.get))(expr.o)
//        EndpointName[Post](succ(comm.receiver.get.asName.endpoint))(expr.o)
      case _ => expr.rewriteDefault()
    }
}
