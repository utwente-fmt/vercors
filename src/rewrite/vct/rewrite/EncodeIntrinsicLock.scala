package vct.col.rewrite

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.check.UnreachableAfterTypeCheck
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import scala.collection.mutable

case object EncodeIntrinsicLock extends RewriterBuilder {
  override def key: String = "intrinsicLock"
  override def desc: String =
    "Encode the intrinsic lock of objects in Java/PVL."

  case class UnlockInvariantFoldFailed(unlock: Unlock[_])
      extends Blame[FoldFailed] {
    override def blame(error: FoldFailed): Unit =
      unlock.blame.blame(UnlockInvariantFailed(unlock, error.failure))
  }

  case class UnlockHeldExhaleFailed(unlock: Unlock[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      unlock.blame.blame(LockTokenNotHeld(unlock, error.failure))
  }

  case class CommitFailedFoldFailed(commit: Commit[_])
      extends Blame[FoldFailed] {
    override def blame(error: FoldFailed): Unit =
      commit.blame.blame(CommitFailed(commit, error.failure))
  }

  case class NotifyAssertFailed(not: Notify[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      not.blame.blame(NotifyFailed(not, error.failure))
  }

  private def LockInvariantOrigin(cls: Class[_]): Origin =
    cls.o.where(prefix = "lockInv")

  private def HeldTokenOrigin(cls: Class[_]): Origin =
    cls.o.where(prefix = "lockHeld")

  private def CommittedOrigin(cls: Class[_]): Origin =
    cls.o.where(prefix = "lockCommitted")

  case class LockLockObjectNull(lock: Lock[_])
      extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit =
      error match {
        case InstanceNull(_) => lock.blame.blame(LockObjectNull(lock))
        case failure: InvocationFailure => PanicBlame("elp").blame(failure)
      }
  }

  case class CommittedLockObjectNull(commit: Committed[_])
      extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit =
      error match {
        case InstanceNull(_) => commit.blame.blame(LockObjectNull(commit))
        case failure: InvocationFailure => PanicBlame("elp").blame(failure)
      }
  }

  case class NotCommittedAssertFailed(lock: Lock[_])
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      lock.blame.blame(LockNotCommitted(lock))
  }

  // Fired when the state is dead after a complete lock operation. PanicBlame
  // guard: Wait(obj) synthesises a PanicBlame'd Lock, whose .blame would throw,
  // so we suppress it — the primary error already fires at the original site.
  case class LockDeadCodeBlame(lock: Lock[_]) extends Blame[RefuteFailed] {
    override def blame(error: RefuteFailed): Unit =
      lock.blame match {
        case _: PanicBlame => // synthesised lock (e.g. from Wait) — suppress
        case _ => lock.blame.blame(LockCodeDead(lock))
      }
  }

  // Fired when the state is dead after the complete unlock operation.
  case class UnlockDeadCodeBlame(unlock: Unlock[_])
      extends Blame[RefuteFailed] {
    override def blame(error: RefuteFailed): Unit =
      unlock.blame.blame(UnlockCodeDead(unlock))
  }

}

case class EncodeIntrinsicLock[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeIntrinsicLock._

  val invariant: SuccessionMap[Class[Pre], InstancePredicate[Post]] =
    SuccessionMap()
  val held: SuccessionMap[Class[Pre], InstancePredicate[Post]] = SuccessionMap()
  val committed: SuccessionMap[Class[Pre], InstanceFunction[Post]] =
    SuccessionMap()
  val needsHeld: mutable.Set[Class[Pre]] = mutable.Set()
  val needsCommitted: mutable.Set[Class[Pre]] = mutable.Set()

  def getClass(obj: Expr[Pre]): ByReferenceClass[Pre] =
    obj.t match {
      case t: TByReferenceClass[Pre] =>
        t.cls.decl.asInstanceOf[ByReferenceClass[Pre]]
      case _ =>
        throw UnreachableAfterTypeCheck(
          "This argument is not a class type.",
          obj,
        )
    }

  def needHeld(e: Expr[Pre]): Unit = {
    needsHeld += getClass(e)
    needsCommitted += getClass(e)
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.foreach {
      case Lock(obj) => needHeld(obj)
      case Unlock(obj) => needHeld(obj)
      case Wait(obj) => needHeld(obj)
      case Notify(obj) => needHeld(obj)
      case Synchronized(obj, _) => needHeld(obj)
      case Held(obj) => needHeld(obj)
      case Committed(obj) => needHeld(obj)
      case Commit(obj) => needHeld(obj)
      case _ =>
    }

    rewriteDefault(program)
  }

  def needsInvariant(cls: ByReferenceClass[Pre]): Boolean =
    cls.intrinsicLockInvariant != tt[Pre]

  def needsInvariant(e: Expr[Pre]): Boolean = needsInvariant(getClass(e))

  def getInvariant(
      obj: Expr[Pre]
  )(implicit o: Origin): InstancePredicateApply[Post] =
    InstancePredicateApply(dispatch(obj), invariant.ref(getClass(obj)), Nil)

  def getHeld(
      obj: Expr[Pre]
  )(implicit o: Origin): InstancePredicateApply[Post] =
    InstancePredicateApply(dispatch(obj), held.ref(getClass(obj)), Nil)

  def getCommitted(obj: Expr[Pre])(
      blame: Blame[InstanceInvocationFailure]
  )(implicit o: Origin): InstanceFunctionInvocation[Post] =
    InstanceFunctionInvocation[Post](
      dispatch(obj),
      committed.ref(getClass(obj)),
      Nil,
      Nil,
      Nil,
      Nil,
    )(blame)

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case cls: ByReferenceClass[Pre] =>
        globalDeclarations.succeed(
          cls,
          cls.rewrite(
            decls =
              classDeclarations.collect {
                if (needsInvariant(cls)) {
                  invariant(cls) = classDeclarations.declare(
                    new InstancePredicate(
                      Nil,
                      Some(dispatch(cls.intrinsicLockInvariant)),
                    )(LockInvariantOrigin(cls))
                  )
                }

                if (needsHeld.contains(cls)) {
                  held(cls) = classDeclarations.declare(
                    new InstancePredicate(Nil, None)(HeldTokenOrigin(cls))
                  )
                }

                if (needsCommitted.contains(cls)) {
                  implicit val o: Origin = CommittedOrigin(cls)
                  committed(cls) = classDeclarations.declare(
                    new InstanceFunction(
                      TBool(),
                      Nil,
                      Nil,
                      None,
                      contract(
                        PanicBlame("empty contract"),
                        decreases = Some(DecreasesClauseAssume[Post]()),
                      ),
                      false,
                    )(AbstractApplicable)
                  )
                }

                cls.decls.foreach(dispatch)
              }._1,
            intrinsicLockInvariant = tt,
          ),
        )
      case other => rewriteDefault(other)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case Held(obj) => Perm(PredicateLocation(getHeld(obj)), WritePerm())
      case c @ Committed(obj) => getCommitted(obj)(CommittedLockObjectNull(c))
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case sync @ Synchronized(obj, body) =>
        TryCatchFinally(
          body = Block(Seq(dispatch(Lock(obj)(sync.blame)), dispatch(body))),
          catches = Nil,
          after = dispatch(Unlock(obj)(sync.blame)),
        )

      case lock @ Lock(obj) =>
        if (needsInvariant(obj))
          Block(Seq(
            Assert(getCommitted(obj)(LockLockObjectNull(lock)))(
              NotCommittedAssertFailed(lock)
            ),
            // Tagged "lockInv" so DetectDeadCode skips it: state is still live here,
            // a check would be a no-op masking the authoritative one below.
            {
              implicit val o: Origin = LockInvariantOrigin(getClass(obj))
              Inhale(Perm(PredicateLocation(getInvariant(obj)), WritePerm()))
            },
            // After unfold, a contradictory invariant (e.g. x>5 ** x<3) is dead here.
            Unfold(ScaledPredicateApply(getInvariant(obj), WritePerm()))(
              PanicBlame(
                "Unfolding a predicate immediately after inhaling it should never fail."
              )
            ),
            // Tagged "lockHeld" so DetectDeadCode skips its generic appendCheck here.
            {
              implicit val o: Origin = HeldTokenOrigin(getClass(obj))
              Inhale(Perm(PredicateLocation(getHeld(obj)), WritePerm()))
            },
            // Distinct from CheckLockInvariantSatisfiability's isolated FramedProof,
            // which catches a universally-false invariant: this catches the invariant
            // contradicting the thread's own state even when satisfiable alone. Both
            // can fire together when the invariant is universally false.
            Refute(ff)(LockDeadCodeBlame(lock)),
          ))
        else
          Block(Seq(
            Assert(getCommitted(obj)(LockLockObjectNull(lock)))(
              NotCommittedAssertFailed(lock)
            ),
            // No invariant to unfold, but the thread's own state (e.g. a contradictory
            // precondition) can still be dead here.
            {
              implicit val o: Origin = HeldTokenOrigin(getClass(obj))
              Inhale(Perm(PredicateLocation(getHeld(obj)), WritePerm()))
            },
            Refute(ff)(LockDeadCodeBlame(lock)),
          ))

      case unlock @ Unlock(obj) =>
        if (needsInvariant(obj))
          Block(Seq(
            Fold(ScaledPredicateApply(getInvariant(obj), WritePerm()))(
              UnlockInvariantFoldFailed(unlock)
            ),
            Exhale(Perm(PredicateLocation(getInvariant(obj)), WritePerm()))(
              PanicBlame(
                "Exhaling a predicate immediately after folding it should never fail."
              )
            ),
            Exhale(Perm(PredicateLocation(getHeld(obj)), WritePerm()))(
              UnlockHeldExhaleFailed(unlock)
            ),
            // Fires UnlockCodeDead if releasing all resources leaves the state dead.
            Refute(ff)(UnlockDeadCodeBlame(unlock)),
            // Tagged "lockCommitted" so DetectDeadCode skips it — the Refute above is
            // the authoritative check.
            {
              implicit val o: Origin = CommittedOrigin(getClass(obj))
              Assume(getCommitted(obj)(PanicBlame(
                "Exhaling held predicate should imply != null"
              )))
            },
          ))
        else
          Block(Seq(
            Exhale(Perm(PredicateLocation(getHeld(obj)), WritePerm()))(
              UnlockHeldExhaleFailed(unlock)
            ),
            // Still check: the locked region may have introduced a contradiction another way.
            Refute(ff)(UnlockDeadCodeBlame(unlock)), {
              implicit val o: Origin = CommittedOrigin(getClass(obj))
              Assume(getCommitted(obj)(PanicBlame(
                "Exhaling held predicate should imply != null"
              )))
            },
          ))

      case wait @ Wait(obj) =>
        dispatch(Block(Seq(
          Unlock(obj)(wait.blame),
          Lock(obj)(PanicBlame("Lock cannot fail after holding the lock")),
        )))

      case notify @ Notify(obj) =>
        Assert(Perm(PredicateLocation(getHeld(obj)), WritePerm()))(
          NotifyAssertFailed(notify)
        )

      case commit @ Commit(obj) =>
        if (needsInvariant(obj))
          Block(Seq(
            Fold(ScaledPredicateApply(getInvariant(obj), WritePerm()))(
              CommitFailedFoldFailed(commit)
            ),
            Exhale(Perm(PredicateLocation(getInvariant(obj)), WritePerm()))(
              PanicBlame(
                "Exhaling a predicate immediately after folding it should never fail."
              )
            ),
            Assume(getCommitted(obj)(PanicBlame(
              "Exhaling invariant predicate should imply != null"
            ))),
          ))
        else
          Assume(getCommitted(obj)(PanicBlame("?")))

      case other => rewriteDefault(other)
    }
  }
}
