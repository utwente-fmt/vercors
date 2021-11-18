package vct.parsers

import vct.col.origin._
import vct.result.VerificationResult

case class ParseError(origin: Origin, message: String) extends VerificationResult.UserError {
  override def text: String = origin.messageInContext(f"Parsing failed: $message")
  override def code: String = "parseError"
}
