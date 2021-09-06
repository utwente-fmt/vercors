package vct.parsers

import vct.col.ast.Origin
import vct.result.VerificationResult

case class ParseError(origin: Origin, message: String) extends VerificationResult.UserError {
  override def text: String = origin.messageInContext(f"Parsing failed: $message")
}
