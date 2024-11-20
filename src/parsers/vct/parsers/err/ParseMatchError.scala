package vct.parsers.err

import vct.result.VerificationError.SystemError

case class ParseMatchError(error: Throwable) extends SystemError {
  initCause(error)
  override def text: String =
    s"A MatchError occurred while parsing. This likely indicates a missing case in a parser (see below): ${error.getMessage}"
}
