package vct.col.err

import vct.col.ast.Expr

case class ContextSensitiveNodeNotResolved(expr: Expr, message: String) extends ASTStateError {
  override def text: String =
    "A node was encountered of which the type is context-sensitive, but its context is not yet resolved. " +
      f"The node says: $message"
}
