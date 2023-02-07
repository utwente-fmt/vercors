package vct.col.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{Assign, Declaration, Deref, DerefVeyMontThread, Node, RunMethod, Statement}
import vct.col.rewrite.veymont.AddVeyMontNodes.AddVeyMontNodesError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

object AddVeyMontNodes extends RewriterBuilder {

  override def key: String = "addVeyMontNodes"

  override def desc: String = "Add nodes for VeyMont specific structures"

  case class AddVeyMontNodesError(node : Node[_], msg: String) extends UserError {
    override def code: String = "addVeyMontNodesError"

    override def text: String = node.o.messageInContext(msg)
  }
}
case class AddVeyMontNodes[Pre <: Generation]() extends Rewriter[Pre] {

  val inRunMethod: ScopedStack[Unit] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case dcl: RunMethod[Pre] => inRunMethod.having(()) {
        rewriteDefault(dcl)
      }
      case _ => rewriteDefault(decl)
    }

  override def dispatch(st: Statement[Pre]): Statement[Post] =
    st match {
      case Assign(target, value) => rewriteDefault(st)
        if(inRunMethod.nonEmpty)
          target match {
            case Deref(obj,_) => obj match {
              case DerefVeyMontThread(ref) => rewriteDefault(st)//target is correct ref to thread
              case _ => throw AddVeyMontNodesError(st,"The target of this assignment must refer to a thread")
            }
            case _ => throw AddVeyMontNodesError(st,"The target of this assignment must be a dereference")
          }
        else rewriteDefault(st)
      case _ => rewriteDefault(st)
    }
}
