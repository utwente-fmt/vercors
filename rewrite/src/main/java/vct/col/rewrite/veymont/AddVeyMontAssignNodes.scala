package vct.col.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{Assign, Declaration, Deref, DerefVeyMontThread, Expr, Node, RunMethod, Statement, VeyMontAssignExpression, VeyMontCommExpression}
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

  val inRunMethod: ScopedStack[Unit] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case dcl: RunMethod[Pre] => inRunMethod.having(()) {
        rewriteDefault(dcl)
      }
      case _ => rewriteDefault(decl)
    }

  override def dispatch(st: Statement[Pre]): Statement[Post] = st match {
      case Assign(target, value) =>
        if(inRunMethod.nonEmpty)
          target match {
            case Deref(obj,_) => obj match {
              case DerefVeyMontThread(ref) => { //target is correct ref to thread
                val derefs = value.collect { case d @ Deref(_,_) => d }.distinct
                if(derefs.isEmpty)
                  new VeyMontAssignExpression[Post](succ(ref.decl),rewriteDefault(st))(st.o)
                else if(derefs.size == 1)
                  derefs.head.obj match {
                    case DerefVeyMontThread(refsender) => new VeyMontCommExpression[Post](succ(ref.decl),succ(refsender.decl),rewriteDefault(st))(st.o)
                    case _ => throw AddVeyMontNodesError(st,"Object identifiers in the value of this assignment can only refer to a thread!")
                  }
                else throw AddVeyMontNodesError(st,"The value of this assignment refers to multiple threads!")
              }
              case _ => throw AddVeyMontNodesError(st,"The target of this assignment must refer to a thread")
            }
            case _ => throw AddVeyMontNodesError(st,"The target of this assignment must be a dereference")
          }
        else rewriteDefault(st)
      case _ => rewriteDefault(st)
    }
}
