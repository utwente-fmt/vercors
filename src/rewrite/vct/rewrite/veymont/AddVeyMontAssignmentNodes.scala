package vct.col.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{Assign, Declaration, Deref, DerefVeyMontThread, Expr, MethodInvocation, Node, ProcedureInvocation, RunMethod, Statement, VeyMontAssignExpression, VeyMontCommExpression, VeyMontSeqProg, VeyMontThread}
import vct.col.ref.Ref
import vct.col.rewrite.veymont.AddVeyMontAssignmentNodes.{AddVeyMontAssignmentError, getDerefsFromExpr, getThreadDeref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

object AddVeyMontAssignmentNodes extends RewriterBuilder {

  override def key: String = "addVeyMontAssignmentNodes"

  override def desc: String = "Add nodes for VeyMont assignments"

  case class AddVeyMontAssignmentError(node : Node[_], msg: String) extends UserError {
    override def code: String = "addVeyMontAssignmentError"

    override def text: String = node.o.messageInContext(msg)
  }

  /**
   * Utility function to extract all derefs from an expression
   * @param exp
   * @tparam Pre
   * @return
   */
  def getDerefsFromExpr[Pre](exp : Expr[Pre]): Seq[Deref[Pre]] = {
    exp.collect { case d@Deref(_, _) => d }.distinct
  }

  /**
   * Utility function to get VeyMontThread from a Deref, and throw error err in case of other dereferences
   * @param deref to be matched
   * @param err to be thrown if match unsuccessful
   * @tparam Pre type of expression
   * @return list of threads that are dereferenced in the expression
   */
  def getThreadDeref[Pre](deref: Deref[Pre], err: UserError) : VeyMontThread[Pre] = deref.obj match {
    case DerefVeyMontThread(threadref) => threadref.decl
    case _ => throw err
  }

}
case class AddVeyMontAssignmentNodes[Pre <: Generation]() extends Rewriter[Pre] {

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
      case _ => throw AddVeyMontAssignmentError(target, "The target of this assignment must refer to a thread")
    }
    case _ => throw AddVeyMontAssignmentError(target, "The target of this assignment must be a dereference to a thread, e.g. someThread.someField")
  }

  /**
   *
   * @param value is the expression to be checked on occurring method calls.
   * @return Whether all method calls of the assignment are method calls on thread objects
   */
  private def checkAssignmentMethodCalls(value: Expr[Pre]): Boolean = {
    !value.exists {
      case m: ProcedureInvocation[Pre] => throw AddVeyMontAssignmentError(m, "Cannot call non-thread method in assignment!")
      case m: MethodInvocation[Pre] => m.obj match {
        case t: DerefVeyMontThread[Pre] => false
        case m => throw AddVeyMontAssignmentError(m, "Cannot call non-thread method in assignment!")
      }
    }
  }

  def createVeyMontAssignNode(receiver: VeyMontThread[Pre], a: Assign[Pre]): Statement[Post] = {
    val derefs = getDerefsFromExpr(a.value)
    if (derefs.isEmpty)
      new VeyMontAssignExpression[Post](succ(receiver), rewriteDefault(a))(a.o)
    else if (derefs.size == 1) {
      val thread = derefs.head.obj match {
        case t: DerefVeyMontThread[Pre] => t
        case _ => throw AddVeyMontAssignmentError(a.value, "The value of this assignment is expected to be a Deref of a thread!")
      }
      if(thread.ref.decl == receiver)
        new VeyMontAssignExpression[Post](succ(receiver),rewriteDefault(a))(a.o)
      else {
        val sender = getAssignmentSender(derefs.head)
        new VeyMontCommExpression[Post](succ(receiver), succ(sender), dispatch(derefs.head.ref.decl.t), rewriteDefault(a))(a.o)
      }
    } else throw AddVeyMontAssignmentError(a.value, "The value of this assignment is not allowed to refer to multiple threads!")
  }

  def getAssignmentSender(deref : Deref[Pre]): VeyMontThread[Pre] =
    getThreadDeref(deref,AddVeyMontAssignmentError (deref, "Object identifiers in the value of this assignment can only refer to a thread!") )

}
