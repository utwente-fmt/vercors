package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.rewrite.util.Extract
import vct.col.util.AstBuildHelpers._
import vct.rewrite.CheckInvariantSatisfiability.SkippableBlame

// Checks that lock invariants are satisfiable. For each by-reference class with a
// non-trivial lock invariant, generates a standalone procedure that inhales the
// invariant and refutes false. If that refute fires, the invariant is contradictory
// and lockInvariantUnsatisfiable is reported. This is independent of whether the
// class is ever locked/unlocked anywhere in the program.
case object CheckLockInvariantSatisfiability
    extends RewriterBuilderArg[Boolean] {
  override def key: String = "checkLockInvSat"
  override def desc: String =
    "Check that lock invariants are not internally contradictory (i.e. unsatisfiable)."

  trait SkippableBlame {
    var shouldSkip: Boolean = false
  }
  // The lock invariant itself (ByReferenceClass.intrinsicLockInvariant) carries no
  // blame, so we anchor the report to an arbitrary ContractApplicable declared in
  // the class (e.g. a method), whose blame was set up when it was parsed from source.
  case class LockInvariantUnsatisfiableBlame(
      cls: ByReferenceClass[_],
      anchor: ContractApplicable[_],
  ) extends Blame[RefuteFailed] with SkippableBlame {
    override def blame(error: RefuteFailed): Unit =
      if (!shouldSkip)
        anchor.blame.blame(LockInvariantUnsatisfiable(cls))
  }

  case class IgnoreWellformednessInInvSat[T <: VerificationFailure](
      blame: SkippableBlame
  ) extends Blame[T] {
    override def blame(error: T): Unit = blame.shouldSkip = true
  }
}

case class CheckLockInvariantSatisfiability[Pre <: Generation](
    doCheck: Boolean = true
) extends Rewriter[Pre] {
  import CheckLockInvariantSatisfiability._

  val wellFormednessBlame: ScopedStack[Blame[VerificationFailure]] =
    ScopedStack()

  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] =
    wellFormednessBlame.topOption.getOrElse(blame)

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case cls: ByReferenceClass[Pre] =>
        super.dispatch(decl)
        if (doCheck && cls.intrinsicLockInvariant != tt[Pre])
          checkLockInvariantSat(cls)
      case _ => super.dispatch(decl)
    }

  private def checkLockInvariantSat(cls: ByReferenceClass[Pre]): Unit = {
    implicit val o: Origin = cls.o.where(prefix = "checkLockInvSat")

    // The default constructor that PVL generates for classes without an explicit
    // constructor has a PanicBlame (it cannot fail), which would not be reported.
    // Pick a declaration that was actually parsed from source instead.
    cls.decls.collectFirst {
      case ca: ContractApplicable[Pre] if !ca.blame.isInstanceOf[PanicBlame] =>
        ca
    } match {
      case None => // No declaration to anchor the report to: nothing to check.
      case Some(anchor) =>
        val extractObj = Extract[Pre]()
        val extracted = extractObj.extract(cls.intrinsicLockInvariant)
        val extractObj.Data(ts, in, inForOut, _, _, _) = extractObj.finish()

        variables.scope {
          localHeapVariables.scope {
            val blame = LockInvariantUnsatisfiableBlame(cls, anchor)
            globalDeclarations.declare(procedure(
              blame = PanicBlame(
                "The postcondition of a lock-invariant-sat check is empty"
              ),
              contractBlame = UnsafeDontCare.Satisfiability(
                "the precondition of a check-lock-inv-sat method is only there to check it."
              ),
              requires = UnitAccountedPredicate(tt)(extracted.o),
              typeArgs = variables.dispatch(ts.keys),
              args = variables
                .dispatch(in.keys ++ inForOut.keys.map(_._1).toSeq),
              body = Some(Scope[Post](
                Nil,
                Block(Seq(
                  Inhale(
                    wellFormednessBlame
                      .having(IgnoreWellformednessInInvSat(blame)) {
                        dispatch(extracted)
                      }
                  ),
                  Refute(ff)(blame),
                )),
              )),
            ))
          }
        }
    }
  }
}
