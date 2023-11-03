package vct.col.util

import vct.col.ast.Node
import vct.col.print.Doc
import vct.result.VerificationError

case class CurrentRewriteNodeContext(node: Node[_]) extends VerificationError.Context {
  override def tryMessageContext(message: String, err: VerificationError): Option[String] =
    err.context[CurrentRewriteProgramContext].map { ctx =>
      Doc.messagesInContext(Seq(
        (ctx.program, node, message)
      ))
    }
}
