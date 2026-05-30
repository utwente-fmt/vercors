package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.rewrite.util.Extract
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case object CheckLoopInvariantSatisfiability extends RewriterBuilderArg[Boolean] {
  override def key: String = "checkInvSat"
  override def desc: String =
    "Check that loop invariants are not internally contradictory (i.e. unsatisfiable)."

  case class AssertPassedInvariantUnsatisfiable(li: LoopInvariant[_])
      extends Blame[ExpectedErrorFailure] {
    override def blame(error: ExpectedErrorFailure): Unit =
      error match {
        case _: ExpectedErrorTrippedTwice =>
        case ExpectedErrorNotTripped(_) =>
          li.blame.blame(LoopInvariantUnsatisfiable(li))
      }
  }

  case class NotWellFormedIgnoreCheckInvSat(err: ExpectedError)
      extends Blame[VerificationFailure] {
    override def blame(error: VerificationFailure): Unit = err.trip(error)
  }
}

case class CheckLoopInvariantSatisfiability[Pre <: Generation](doCheck: Boolean = true)
    extends Rewriter[Pre] {
  import CheckLoopInvariantSatisfiability._

  val expectedErrors: ScopedStack[ArrayBuffer[ExpectedError]] = ScopedStack()
  val wellFormednessBlame: ScopedStack[Blame[VerificationFailure]] = ScopedStack()

  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] =
    wellFormednessBlame.topOption.getOrElse(blame)

  def checkInvariant(li: LoopInvariant[Pre]): Unit = {
    implicit val o: Origin = li.o.where(prefix = "checkInvSat")
    li.invariant match {
      case BooleanValue(false) =>
      // Assume the invariant is intentionally false (already always an error)
      case inv =>
        val err = ExpectedError(
          "assertFailed:false",
          o,
          AssertPassedInvariantUnsatisfiable(li),
        )
        val onlyAssertBlame = FilterExpectedErrorBlame(
          PanicBlame("A boolean assert can only report assertFailed:false"),
          err,
        )
        expectedErrors.top += err
        val extractObj = Extract[Pre]()
        val extracted = extractObj.extract(inv)
        val extractObj.Data(ts, in, _, _, _) = extractObj.finish()
        variables.scope {
          localHeapVariables.scope {
            globalDeclarations.declare(procedure(
              blame = PanicBlame(
                "The postcondition of a method checking loop invariant satisfiability is empty"
              ),
              contractBlame = UnsafeDontCare.Satisfiability(
                "the precondition of a check-inv-sat method is only there to check it."
              ),
              requires = UnitAccountedPredicate(
                wellFormednessBlame.having(NotWellFormedIgnoreCheckInvSat(err)) {
                  dispatch(extracted)
                }
              )(extracted.o),
              typeArgs = variables.dispatch(ts.keys),
              args = variables.dispatch(in.keys),
              body = Some(Scope[Post](Nil, Assert(ff)(onlyAssertBlame))),
            ))
          }
        }
    }
  }

  override def dispatch(verification: Verification[Pre]): Verification[Post] = {
    val (errs, tasks) = expectedErrors.collect {
      verification.tasks.map(dispatch)
    }
    Verification(tasks, errs ++ verification.expectedErrors)(verification.o)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    if (!doCheck) stat.rewriteDefault()
    else stat match {
      case loop @ Loop(_, _, _, li @ LoopInvariant(_, _), _) =>
        checkInvariant(li)
        loop.rewriteDefault()
      case other => other.rewriteDefault()
    }
}
