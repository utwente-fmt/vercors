package vct.col.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{Assign, Declaration, Deref, DerefVeyMontThread, Expr, MethodInvocation, Node, ProcedureInvocation, RunMethod, Statement, VeyMontAssignExpression, VeyMontCommExpression, VeyMontSeqProg, VeyMontThread}
import vct.col.ref.Ref
import vct.col.rewrite.veymont.AddVeyMontAssignNodes.AddVeyMontNodesError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

object AddVeyMontAssignNodes extends RewriterBuilder {

  override def key: String = "addVeyMontNodes"

  override def desc: String = "Add nodes for VeyMont specific structures"

  case class AddVeyMontNodesError(node : Node[_], msg: String) extends UserError {
    override def code: String = "addVeyMontNodesError"

    override def text: String = node.o.messageInContext(msg)
  }

}
case class AddVeyMontAssignNodes[Pre <: Generation]() extends Rewriter[Pre] {

  val inSeqProg: ScopedStack[Unit] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case dcl: VeyMontSeqProg[Pre] => inSeqProg.having(()) {
        rewriteDefault(dcl)
      }
      case _ => rewriteDefault(decl)
    }

  override def dispatch(st: Statement[Pre]): Statement[Post] = st match {
      case a@Assign(target, value) =>
        if(inSeqProg.nonEmpty) {
          val receiver = getAssignmentReceiver(target)
          checkAssignmentMethodCalls(value)
          createVeyMontAssignNode(receiver,a)
        } else rewriteDefault(st)
      case _ => rewriteDefault(st)
    }

  def getAssignmentReceiver(target: Expr[Pre]): VeyMontThread[Pre] = target match {
    case Deref(obj, _) => obj match {
      case DerefVeyMontThread(ref) =>  ref.decl//target is correct ref to thread
      case _ => throw AddVeyMontNodesError(target, "The target of this assignment must refer to a thread")
    }
    case _ => throw AddVeyMontNodesError(target, "The target of this assignment must be a dereference to a thread, e.g. someThread.someField")
  }

  /**
   *
   * @param value is the expression to be checked on occurring method calls.
   * @return Whether all method calls of the assignment are method calls on thread objects
   */
  private def checkAssignmentMethodCalls(value: Expr[Pre]): Boolean = {
    !value.exists {
      case m: ProcedureInvocation[Pre] => throw AddVeyMontNodesError(m, "Cannot call non-thread method in assignment!")
      case m: MethodInvocation[Pre] => m.obj match {
        case t: DerefVeyMontThread[Pre] => false
        case m => throw AddVeyMontNodesError(m, "Cannot call non-thread method in assignment!")
      }
    }
  }

  def createVeyMontAssignNode(receiver: VeyMontThread[Pre], a: Assign[Pre]): Statement[Post] = {
    val derefs = a.value.collect { case d@Deref(_, _) => d }.distinct
    if (derefs.isEmpty)
      new VeyMontAssignExpression[Post](succ(receiver), rewriteDefault(a))(a.o)
    else if (derefs.size == 1) {
      val sender = getAssignmentSender(derefs.head)
      new VeyMontCommExpression[Post](succ(receiver), succ(sender), rewriteDefault(a))(a.o)
    } else throw AddVeyMontNodesError(a.value, "The value of this assignment is not allowed to refer to multiple threads!")
  }

  def getAssignmentSender(deref : Deref[Pre]): VeyMontThread[Pre] = deref.obj match {
    case DerefVeyMontThread(refsender) => refsender.decl
    case _ => throw AddVeyMontNodesError(deref, "Object identifiers in the value of this assignment can only refer to a thread!")
  }
}
