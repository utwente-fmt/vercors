package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteVerificationContext
import vct.col.ast._
import vct.col.rewrite.RefuteToInvertedAssert.AssertPassedRefuteFailed
import vct.col.origin.{Blame, ExpectedError, ExpectedErrorFailure, ExpectedErrorNotTripped, ExpectedErrorTrippedTwice, FilterExpectedErrorBlame, NontrivialUnsatisfiable, Origin, PanicBlame, RefuteFailed}
import vct.col.rewrite._
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case object RefuteToInvertedAssert extends RewriterBuilder {
  override def key: String = "refute"
  override def desc: String = "Translate refute to an assertion of which the error output is inverted"

  case class AssertPassedRefuteFailed(refute: Refute[_]) extends Blame[ExpectedErrorFailure] {
    override def blame(error: ExpectedErrorFailure): Unit = error match {
      case err: ExpectedErrorTrippedTwice =>
        PanicBlame("A single assert cannot trip twice").blame(err)
      case ExpectedErrorNotTripped(_) =>
        refute.blame.blame(RefuteFailed(refute))
    }
  }
}

case class RefuteToInvertedAssert[Pre <: Generation]() extends Rewriter[Pre] {
  val expectedErrors: ScopedStack[ArrayBuffer[ExpectedError]] = ScopedStack[ArrayBuffer[ExpectedError]]()

  override def dispatch(verification: Verification[Pre]): Verification[Post] = {
    val (errs, tasks) = expectedErrors.collect {
      verification.tasks.map(dispatch)
    }
    // PB: Important: the expected errors from this pass must appear before other errors, since the absence of an assert
    // failure from this pass may in turn indicate an expected "unsatisfiable" error from an earlier pass.
    Verification(tasks, errs ++ verification.expectedErrors)(verification.o)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case refute @ Refute(assn) =>
      implicit val o: Origin = stat.o
      val err = ExpectedError("assertFailed:.*", stat.o, AssertPassedRefuteFailed(refute))
      expectedErrors.top += err
      IndetBranch(Seq(
        Block(Seq(
          Assert(dispatch(assn))(FilterExpectedErrorBlame(PanicBlame("wrong assert error kind"), err)),
          Inhale(ff),
        )),
        Block(Nil),
      ))

    case other => rewriteDefault(other)
  }
}
