package vct.parsers.err

import vct.result.VerificationError.SystemError

case class ParseMatchError(objectDescription: String) extends SystemError {
  override def text: String =
    s"A MatchError occurred while parsing. This likely indicates a missing case in a parser: $objectDescription"
}
