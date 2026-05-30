package vct.rewrite

import hre.util.ScopedStack
import scala.collection.mutable
import vct.col.ast._
import vct.col.check.UnreachableAfterTypeCheck
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.rewrite.util.Extract
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case object CheckLockInvariantSatisfiability extends RewriterBuilder {
  override def key: String = "checkLockInvSat"
  override def desc: String =
    "Check that lock invariants are not internally contradictory (i.e. unsatisfiable)."

  case class AssertPassedLockInvariantUnsatisfiable[G](stmt: Statement[G])
      extends Blame[ExpectedErrorFailure] {
    override def blame(error: ExpectedErrorFailure): Unit =
      error match {
        case _: ExpectedErrorTrippedTwice =>
        case ExpectedErrorNotTripped(_) =>
          stmt match {
            case lock: Lock[G] => lock.blame.blame(LockInvariantUnsatisfiable(lock))
            case unlock: Unlock[G] => unlock.blame.blame(LockInvariantUnsatisfiable(unlock))
            case _ =>
          }
      }
  }

  case class NotWellFormedIgnoreCheckLockInvSat(err: ExpectedError)
      extends Blame[VerificationFailure] {
    override def blame(error: VerificationFailure): Unit = err.trip(error)
  }
}

case class CheckLockInvariantSatisfiability[Pre <: Generation]() extends Rewriter[Pre] {
  import CheckLockInvariantSatisfiability._

  val checkedClasses: mutable.Set[ByReferenceClass[Pre]] = mutable.Set()
  val expectedErrors: ScopedStack[ArrayBuffer[ExpectedError]] = ScopedStack()
  val wellFormednessBlame: ScopedStack[Blame[VerificationFailure]] = ScopedStack()

  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] =
    wellFormednessBlame.topOption.getOrElse(blame)

  def getClass(obj: Expr[Pre]): ByReferenceClass[Pre] =
    obj.t match {
      case t: TByReferenceClass[Pre] => t.cls.decl.asInstanceOf[ByReferenceClass[Pre]]
      case _ =>
        throw UnreachableAfterTypeCheck("Lock target is not a by-reference class.", obj)
    }

  def checkLockInvariant(inv: Expr[Pre], stmt: Statement[Pre])(implicit o: Origin): Unit = {
    val err = ExpectedError(
      "assertFailed:false",
      o,
      AssertPassedLockInvariantUnsatisfiable(stmt),
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
            "The postcondition of a method checking lock invariant satisfiability is empty"
          ),
          contractBlame = UnsafeDontCare.Satisfiability(
            "the precondition of a check-lock-inv-sat method is only there to check it."
          ),
          requires = UnitAccountedPredicate(
            wellFormednessBlame.having(NotWellFormedIgnoreCheckLockInvSat(err)) {
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

  override def dispatch(verification: Verification[Pre]): Verification[Post] = {
    val (errs, tasks) = expectedErrors.collect {
      verification.tasks.map(dispatch)
    }
    Verification(tasks, errs ++ verification.expectedErrors)(verification.o)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case lock @ Lock(obj) =>
        val cls = getClass(obj)
        if (cls.intrinsicLockInvariant != tt[Pre] && !checkedClasses.contains(cls)) {
          checkedClasses += cls
          implicit val o: Origin = lock.o.where(prefix = "checkLockInvSat")
          checkLockInvariant(cls.intrinsicLockInvariant, lock)
        }
        lock.rewriteDefault()

      case unlock @ Unlock(obj) =>
        val cls = getClass(obj)
        if (cls.intrinsicLockInvariant != tt[Pre] && !checkedClasses.contains(cls)) {
          checkedClasses += cls
          implicit val o: Origin = unlock.o.where(prefix = "checkLockInvSat")
          checkLockInvariant(cls.intrinsicLockInvariant, unlock)
        }
        unlock.rewriteDefault()

      case other => other.rewriteDefault()
    }
}
