package vct.parsers

import vct.col.origin._
import vct.result.VerificationError

case class ParseError(origin: Origin, message: String) extends VerificationError.UserError {
  override def text: String = origin.messageInContext(f"Parsing failed: $message")
  override def code: String = "parseError"
}
