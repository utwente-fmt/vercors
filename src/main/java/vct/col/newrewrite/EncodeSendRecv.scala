package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.ast._
import vct.col.newrewrite.EncodeSendRecv.{DuplicateRecv, WrongSendRecvPosition}
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.result.VerificationResult.UserError

import scala.collection.mutable

case object EncodeSendRecv extends RewriterBuilder {
  case class WrongSendRecvPosition(stat: Statement[_]) extends UserError {
    override def code: String = "wrongSendRecv"
    override def text: String =
      stat.o.messageInContext("Send and recv may only occur within a parallel block, or loop with an iteration contract, and must be unconditionally executed.")
  }

  case class IncorrectSendDependency(dependency: Local[_]) extends UserError {
    override def code: String = "sendResDep"
    override def text: String =
      dependency.o.messageInContext("This expression may not be constant with respect to the parallel block, or loop with an iteration contract.")
  }

  case class DuplicateRecv(left: Recv[_], right: Recv[_]) extends UserError {
    override def code: String = "dupRecv"
    override def text: String =
      Origin.messagesInContext(Seq(
        (left.o, "There must be at most one recv statement for a given send, but there is one here ..."),
        (right.o, "... and here."),
      ))
  }
}

case class EncodeSendRecv[Pre <: Generation]() extends Rewriter[Pre] {
  val sendOfDecl: mutable.Map[SendDecl[Pre], Send[Pre]] = mutable.Map()
  val recvOfDecl: mutable.Map[SendDecl[Pre], Recv[Pre]] = mutable.Map()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    program.transSubnodes.foreach { case send: Send[Pre] => sendOfDecl(send.decl) = send; case _ => }
    program.rewrite()
  }

  val allowSendRecv: ScopedStack[Option[Variable[Pre]]] = ScopedStack()
  allowSendRecv.push(None)

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case block: Block[Pre] => rewriteDefault(block)
    case label: Label[Pre] => rewriteDefault(label)

    case Send(_, _, res) =>
      if(allowSendRecv.top.isEmpty) throw WrongSendRecvPosition(stat)
      else Exhale(freshSuccessionScope { dispatch(res) })(???)(stat.o)

    case recv @ Recv(Ref(decl)) =>
      val send = sendOfDecl(decl)

      recvOfDecl.get(decl) match {
        case Some(value) => throw DuplicateRecv(value, recv)
        case None => recvOfDecl(decl) = recv
      }

      if(allowSendRecv.top.isEmpty) throw WrongSendRecvPosition(stat)
      else Inhale(???)(recv.o)


    case other => allowSendRecv.having(None) { rewriteDefault(other) }
  }

  override def dispatch(parRegion: ParRegion[Pre]): ParRegion[Post] = parRegion match {
    case block @ ParBlock(_, iters, _, _, _, _) =>
      iters match {
        case Seq(v) => allowSendRecv.having(Some(v.variable)) { block.rewrite() }
        case _ => block.rewrite()
      }
    case other => rewriteDefault(other)
  }
}
