package vct.col.util

import vct.col.origin._

case class ExpectedError(errorCode: String, innerBlame: Blame[VerificationFailure]) extends Blame[VerificationFailure] {
  var tripped: Boolean = false

  def trip(): Unit = {
    if(tripped) innerBlame.blame(ExpectedErrorTrippedTwice(this))
    else tripped = true
  }

  override def blame(error: VerificationFailure): Unit =
    if(error.code == errorCode) trip()
    else innerBlame.blame(error)
}
