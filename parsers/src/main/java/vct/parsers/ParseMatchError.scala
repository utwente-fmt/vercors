package vct.parsers

import vct.result.VerificationResult.SystemError

case class ParseMatchError(objectDescription: String) extends SystemError {
  override def text: String =
    s"A MatchError occurred while parsing. This likely indicates a missing case in a parser: $objectDescription"
}
