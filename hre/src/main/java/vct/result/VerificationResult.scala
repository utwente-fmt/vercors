package vct.result

sealed trait VerificationResult extends Throwable {
  def text: String
}

object VerificationResult {
  /* Malformed input, unsupported features, and any other documented deficiencies in VerCors */
  trait UserError extends VerificationResult

  /* Some state was reached that was not expected. A SystemError is *always* a bug. If it is an expected failure, it
   * should be a (documented) UserError. */
  trait SystemError extends VerificationResult

  case class Unreachable(text: String) extends SystemError

  /* Verification completed. Any failures have been reported to the appropriate Blame. */
  case object Ok extends VerificationResult {
    override def text: String = "Verification completed normally."
  }
}