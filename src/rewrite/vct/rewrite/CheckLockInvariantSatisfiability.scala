package vct.rewrite

import scala.collection.mutable
import vct.col.ast._
import vct.col.check.UnreachableAfterTypeCheck
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

// Checks that lock invariants are satisfiable. On the first lock or unlock for a given
// class, generates an isolated FramedProof that inhales the invariant and refutes false.
// If the invariant is contradictory, refute fires and lockInvariantUnsatisfiable is reported.
case object CheckLockInvariantSatisfiability extends RewriterBuilder {
  override def key: String = "checkLockInvSat"
  override def desc: String =
    "Check that lock invariants are not internally contradictory (i.e. unsatisfiable)."

  case class LockInvariantUnsatisfiableBlame(stmt: Statement[_])
      extends Blame[RefuteFailed] {
    override def blame(error: RefuteFailed): Unit =
      stmt match {
        case lock: Lock[_]     => lock.blame.blame(LockInvariantUnsatisfiable(lock))
        case unlock: Unlock[_] => unlock.blame.blame(LockInvariantUnsatisfiable(unlock))
        case _                 =>
      }
  }
}

case class CheckLockInvariantSatisfiability[Pre <: Generation]() extends Rewriter[Pre] {
  import CheckLockInvariantSatisfiability._

  val checkedClasses: mutable.Set[ByReferenceClass[Pre]] = mutable.Set()

  def getClass(obj: Expr[Pre]): ByReferenceClass[Pre] =
    obj.t match {
      case t: TByReferenceClass[Pre] => t.cls.decl.asInstanceOf[ByReferenceClass[Pre]]
      case _ =>
        throw UnreachableAfterTypeCheck("Lock target is not a by-reference class.", obj)
    }

  private def satCheck(inv: Expr[Pre], stmt: Statement[Pre])(implicit
      o: Origin
  ): Statement[Post] =
    Extract(
      FramedProof(
        tt,
        Block[Post](Seq[Statement[Post]](
          Inhale(dispatch(inv)),
          Refute(ff)(LockInvariantUnsatisfiableBlame(stmt)),
        )),
        tt,
      )(PanicBlame("FramedProof with trivial pre/post cannot fail")),
      None,
    )(PanicBlame("Extract without termination measure cannot fail"))

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case lock @ Lock(obj) =>
        val cls = getClass(obj)
        if (cls.intrinsicLockInvariant != tt[Pre] && !checkedClasses.contains(cls)) {
          checkedClasses += cls
          implicit val o: Origin = lock.o.where(prefix = "checkLockInvSat")
          Block[Post](Seq[Statement[Post]](satCheck(cls.intrinsicLockInvariant, lock), lock.rewriteDefault()))
        } else lock.rewriteDefault()

      case unlock @ Unlock(obj) =>
        val cls = getClass(obj)
        if (cls.intrinsicLockInvariant != tt[Pre] && !checkedClasses.contains(cls)) {
          checkedClasses += cls
          implicit val o: Origin = unlock.o.where(prefix = "checkLockInvSat")
          Block[Post](Seq[Statement[Post]](satCheck(cls.intrinsicLockInvariant, unlock), unlock.rewriteDefault()))
        } else unlock.rewriteDefault()

      case other => other.rewriteDefault()
    }
}
