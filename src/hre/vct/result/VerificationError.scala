package vct.result

sealed abstract class VerificationError extends RuntimeException {
  def text: String

  override def getMessage: String = text
}

object VerificationError {
  /* Malformed input, unsupported features, and any other documented deficiencies in VerCors */
  abstract class UserError extends VerificationError {
    def code: String
  }

  /* Some state was reached that was not expected. A SystemError is *always* a bug. If it is an expected failure, it
   * should be a (documented) UserError. */
  abstract class SystemError extends VerificationError

  case class Unreachable(text: String) extends SystemError
}