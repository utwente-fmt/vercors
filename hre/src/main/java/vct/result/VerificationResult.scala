package vct.result

sealed abstract class VerificationResult extends RuntimeException {
  def text: String
}

object VerificationResult {
  /* Malformed input, unsupported features, and any other documented deficiencies in VerCors */
  abstract class UserError extends VerificationResult {
    def code: String
  }

  case class ExcludedByCompilationError(message: String) extends UserError {
    override def text: String = s"$message " +
      "VerCors normally assumes that the example compiles. " +
      "If the example does not compile, errors from VerCors may not be very clear."

    override def code: String = "excludedByCompilation"
  }

  /* Some state was reached that was not expected. A SystemError is *always* a bug. If it is an expected failure, it
   * should be a (documented) UserError. */
  abstract class SystemError extends VerificationResult

  case class Unreachable(text: String) extends SystemError

  /* Verification completed. Any failures have been reported to the appropriate Blame. */
  case object Ok extends VerificationResult {
    override def text: String = "Verification completed normally."
  }
}