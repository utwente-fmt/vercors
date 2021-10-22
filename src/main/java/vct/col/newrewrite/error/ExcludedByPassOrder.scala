package vct.col.newrewrite.error

import vct.col.ast.Node
import vct.result.VerificationResult.SystemError

case class ExcludedByPassOrder(message: String, node: Option[Node]) extends SystemError {
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
