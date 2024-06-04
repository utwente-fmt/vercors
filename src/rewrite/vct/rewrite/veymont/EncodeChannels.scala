package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  Assign,
  Block,
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
  TClass,
  TVar,
  ThisObject,
  Type,
  Value,
  Variable,
}
import vct.col.origin.{Name, Origin, PanicBlame, SourceName}
import vct.col.ref.Ref
import vct.col.rewrite.adt.ImportADTImporter
import vct.col.rewrite.{
  Generation,
  Rewriter,
  RewriterBuilder,
  RewriterBuilderArg,
}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable
import scala.reflect.ClassTag

object EncodeChannels extends RewriterBuilder {
  override def key: String = "encodeChannels"

  override def desc: String =
    "Encodes channels using plain assignment. Encodes channel invariants using exhale/inhale."
}

case class EncodeChannels[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {
  val currentMsg = ScopedStack[Expr[Post]]()

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
        val m = new Variable(dispatch(comm.msg.t))(comm.o.where(name = "m"))
        Scope(
          Seq(m),
          Block(Seq(
            assignLocal(m.get, dispatch(comm.msg)),
            Exhale(currentEndpoint.having(comm.sender.get.decl) {
              foldAny(comm.invariant.t)(unfoldStar(comm.invariant).map { e =>
                EndpointExpr[Post](succ(comm.sender.get.decl), dispatch(e))
              })
            })(PanicBlame("TODO: Redirect failing exhale")),
            Inhale(currentEndpoint.having(comm.receiver.get.decl) {
              dispatch(comm.invariant)
              foldAny(comm.invariant.t)(unfoldStar(comm.invariant).map { e =>
                EndpointExpr[Post](succ(comm.receiver.get.decl), dispatch(e))
              })
            }),
            Assign(dispatch(comm.target), m.get)(PanicBlame(
              "TODO: Redirect to comm?"
            )),
          )),
        )
      case _ => statement.rewriteDefault()
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case InEndpoint(_, endpoint, Perm(loc, perm)) =>
        ChorPerm[Post](succ(endpoint), dispatch(loc), dispatch(perm))(expr.o)
      // TODO: Check this in the check pass
      case InEndpoint(_, endpoint, cp: ChorPerm[Pre]) => assert(false); ???
      case _ => expr.rewriteDefault()
    }
}
