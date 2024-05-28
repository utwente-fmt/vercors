package hre.progress

import vct.result.VerificationError.SystemError

class ProgressLogicError(trail: Seq[String], amount: Double)
    extends SystemError {
  override def text: String =
    s"The amount of progress ($amount) exceeds the range [0; 1] at trail: ${trail
        .reverse.mkString(" > ")}"
}
