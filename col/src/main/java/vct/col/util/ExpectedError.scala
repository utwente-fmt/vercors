package vct.col.util

import vct.col.origin._

object ExpectedError {
  def apply(errorCode: String, errorRegion: Origin, blame: Blame[ExpectedErrorFailure]): ExpectedError =
    new ExpectedError(errorCode, errorRegion, blame)
}

class ExpectedError(val errorCode: String, val errorRegion: Origin, val blame: Blame[ExpectedErrorFailure]) {
  var tripped: Option[VerificationFailure] = None

  def trip(failure: VerificationFailure): Unit =
    tripped match {
      case None => tripped = Some(failure)
      case Some(leftFailure) =>
        blame.blame(ExpectedErrorTrippedTwice(this, leftFailure, failure))
    }

  def signalDone(): Unit =
    if(tripped.isEmpty) {
      blame.blame(ExpectedErrorNotTripped(this))
    }
}
