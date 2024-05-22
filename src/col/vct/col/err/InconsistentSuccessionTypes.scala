package vct.col.err

import vct.col.ast.Declaration
import vct.result.Message
import vct.result.VerificationError.SystemError

case class InconsistentSuccessionTypes(left: Declaration[_], right: Declaration[_]) extends SystemError {
  override def text: String = {
    val leftContext = context[vct.col.util.CurrentRewriteProgramContext].getOrElse(left.o)

    Message.messagesInContext(
      leftContext -> "The kind of this declaration does not match ...",
      right.highlight(right) -> "... the kind of this declaration, so it may not be succeeded by this declaration.",
    )
  }
}
