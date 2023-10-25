package vct.col.util

import vct.col.ast.Node
import vct.col.print.Doc
import vct.result.VerificationError

case class CurrentCheckNodeContext(node: Node[_]) extends VerificationError.Context {
  override def tryMessageContext(message: String, err: VerificationError): Option[String] =
    err.context[CurrentCheckProgramContext].map { ctx =>
      Doc.messagesInContext(Seq(
        (ctx.program, node, message)
      ))
    }
}
