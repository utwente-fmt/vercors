package vct.col.failure

sealed trait FailureResult {
  def verdict: Option[Any]
}

object BooleanFailureResult {
  // PB: note that Fail is typically returned by error canaries, and Unknown by the backend.
  sealed trait Verdict
  case object Fail extends Verdict
  case object Unknown extends Verdict
}

case class BooleanFailureResult(verdict: Option[BooleanFailureResult.Verdict], failure: PotentialBooleanFailure) extends FailureResult

object AssertionFailureResult {
  // PB: note that Fail is typically returned by error canaries, and Unknown by the backend.
  sealed trait Verdict
  case class Fail(reason: Reason) extends Verdict
  case class Unknown(reason: Reason) extends Verdict
}

case class AssertionFailureResult(verdict: Option[AssertionFailureResult.Verdict], failure: PotentialBooleanFailure) extends FailureResult