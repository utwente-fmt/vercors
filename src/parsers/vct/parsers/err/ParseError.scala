package vct.parsers.err

import vct.col.origin._
import vct.result.{Message, VerificationError}

case class ParseError(origin: Origin, message: String) extends VerificationError.UserError {
  override def text: String = origin.messageInContext(f"Parsing failed: $message")
  override def code: String = "parseError"
}

case class ParseErrors(errors: Seq[ParseError]) extends VerificationError.UserError {
  override def text: String = Message.messagesInContext(errors.map(err => err.origin -> s"Parsing failed: ${err.message}"): _*)
  override def code: String = "parseError"
}