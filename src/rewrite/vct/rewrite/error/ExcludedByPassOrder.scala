package vct.col.rewrite.error

import vct.col.ast.Node
import vct.result.VerificationError.SystemError

case class ExcludedByPassOrder(message: String, node: Option[Node[_]]) extends SystemError {
  override def text: String = {
    node match {
      case Some(node) =>
        node.o.messageInContext("An AST node or a property of the AST was reached, " +
          "that should have been excluded by an earlier pass. " + message)
      case None =>
        "An AST node or a property of the AST was reached, " +
          "that should have been excluded by an earlier pass. " + message
    }
  }
}

object ExtraNode extends ExcludedByPassOrder(
  "Language-specific type, expression or declaration was encountered, which should have been already resolved", None)