package vct.col.util

import vct.col.ast.Program
import vct.col.print.Doc
import vct.result.VerificationError

case class CurrentCheckProgramContext(program: Program[_]) extends CurrentProgramContext {
  override def tryMessageContext(message: String, err: VerificationError): Option[String] =
    Some(Doc.messagesInContext(Seq(
      (program, program, message)
    )))
}
