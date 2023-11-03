package vct.col.rewrite.error

import vct.col.ast.Node
import vct.col.util.CurrentRewriteProgramContext
import vct.result.VerificationError.SystemError

case class ExcludedByPassOrder(message: String, node: Option[Node[_]]) extends SystemError {
  override def text: String = {
    val fullMessage =
      "An AST node or a property of the AST was reached, " +
        "that should have been excluded by an earlier pass. " +
        message

    node match {
      case Some(node) =>
        context[CurrentRewriteProgramContext] match {
          case Some(ctx) => ctx.program.messageInContext(node, fullMessage)
          case None => node.o.messageInContext(fullMessage)
        }
      case None => messageContext(fullMessage)
    }
  }
}

object ExtraNode extends ExcludedByPassOrder(
  "Language-specific type, expression or declaration was encountered, which should have been already resolved", None)