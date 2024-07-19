package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  Assert,
  Assign,
  Block,
  ByValueClass,
  ChorStatement,
  BooleanValue,
  ChorExpr,
  ChorPerm,
  ChorRun,
  Choreography,
  Class,
  Communicate,
  CommunicateStatement,
  Constructor,
  ConstructorInvocation,
  Declaration,
  Deref,
  Endpoint,
  EndpointExpr,
  EndpointName,
  EndpointStatement,
  Eval,
  Exhale,
  Expr,
  FieldLocation,
  Inhale,
  InstanceField,
  InstanceMethod,
  InstancePredicate,
  IterationContract,
  Local,
  LoopContract,
  LoopInvariant,
  Message,
  Perm,
  Program,
  Receiver,
  Result,
  Scope,
  Sender,
  Statement,
  TByValueClass,
  TClass,
  TVar,
  ThisObject,
  Type,
  Value,
  Variable,
}
import vct.col.origin.{
  AssertFailed,
  AssignLocalOk,
  Blame,
  ChannelInvariantNotEstablished,
  ChannelInvariantNotEstablishedLocally,
  ExhaleFailed,
  Name,
  Origin,
  PanicBlame,
  SourceName,
}
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.adt.ImportADTImporter
import vct.col.rewrite.{
  Generation,
  Rewriter,
  RewriterBuilder,
  RewriterBuilderArg,
}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.rewrite.veymont.EncodeChannels.{
  AssertFailedToChannelInvariantNotEstablished,
  ExhaleFailedToChannelInvariantNotEstablished,
}
import vct.rewrite.veymont.EncodeChoreography.AssertFailedToParticipantsNotDistinct

import scala.collection.mutable
import scala.reflect.ClassTag

object EncodeChannels extends RewriterBuilder {
  override def key: String = "encodeChannels"

  override def desc: String =
    "Encodes channels using plain assignment. Encodes channel invariants using exhale/inhale."

  case class AssertFailedToChannelInvariantNotEstablished(comm: Communicate[_])
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      comm.blame.blame(ChannelInvariantNotEstablished(error.failure, comm))
  }

  case class ExhaleFailedToChannelInvariantNotEstablished(comm: Communicate[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      comm.blame
        .blame(ChannelInvariantNotEstablishedLocally(error.failure, comm))
  }
}

case class EncodeChannels[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {
  val msgSucc = SuccessionMap[Communicate[Pre], Variable[Post]]()
  val includeChorExpr = ScopedStack[Boolean]()
  val substitutions = ScopedStack[Map[Expr[Pre], Expr[Post]]]()

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

        Scope(
          Seq(m),
          Block(Seq(
            assignLocal(
              m.get,
              EndpointExpr[Post](succ(sender), dispatch(comm.msg)),
            ),
            Assert(currentEndpoint.having(comm.sender.get.decl) {
              includeChorExpr.having(true) {
                EndpointExpr[Post](succ(sender), dispatch(comm.invariant))
              }
            })(AssertFailedToChannelInvariantNotEstablished(comm)),
            Exhale(currentEndpoint.having(comm.sender.get.decl) {
              includeChorExpr.having(false) {
                foldAny(comm.invariant.t)(unfoldStar(comm.invariant).map { e =>
                  EndpointExpr[Post](succ(sender), dispatch(e))
                })
              }
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
              ))) {
                includeChorExpr.having(true) {
                  foldAny(comm.invariant.t)(unfoldStar(comm.invariant).map {
                    case e: ChorExpr[Pre] => e.rewriteDefault()
                    case e => EndpointExpr[Post](succ(receiver), dispatch(e))
                  })
                }
              }
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
      case InEndpoint(_, endpoint, cp: ChorPerm[Pre]) => assert(false); ???
      case Message(Ref(comm)) => Local[Post](msgSucc.ref(comm))(comm.o)
      case Sender(Ref(comm)) =>
        EndpointName[Post](succ(comm.sender.get.decl))(expr.o)
      case Receiver(Ref(comm)) =>
        EndpointName[Post](succ(comm.receiver.get.decl))(expr.o)
      case chor: ChorExpr[Pre] if includeChorExpr.topOption.contains(true) =>
        // Case chor must be kept: include the expression. The top level invariant rewrite will wrap
        // it into a proper EndpointExpr, hence, remove the \chor layer
        chor.expr.rewriteDefault()
      case chor: ChorExpr[Pre] if includeChorExpr.topOption.contains(false) =>
        // Case chor must not be kept: exhaling the invariant. Since the validity of the invariant, including
        // the chor part, is already established, it doesn't need to be included when exhaling
        BooleanValue(true)(chor.o)
      case chor: ChorExpr[Pre] =>
        // Case no boolean in includeChor: just rewrite the expression plainly and keep the chor expr
        // This also happens to be the case we need for the inhale case, where we just want the plain \chor expr
        // Such that all the necessary wrapped perms will be unwrapped by the EncodePermissionStratification phase
        chor.rewriteDefault()
      case _ => expr.rewriteDefault()
    }
}
