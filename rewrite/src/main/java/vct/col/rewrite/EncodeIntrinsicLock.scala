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
  override def desc: String = "Encode the intrinsic lock of objects in Java/PVL."

  case class UnlockInvariantFoldFailed(unlock: Unlock[_]) extends Blame[FoldFailed] {
    override def blame(error: FoldFailed): Unit =
      unlock.blame.blame(UnlockInvariantFailed(unlock, error.failure))
  }

  case class UnlockHeldExhaleFailed(unlock: Unlock[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      unlock.blame.blame(LockTokenNotHeld(unlock, error.failure))
  }

  case class CommitFailedFoldFailed(commit: Commit[_]) extends Blame[FoldFailed] {
    override def blame(error: FoldFailed): Unit =
      commit.blame.blame(CommitFailed(commit, error.failure))
  }

  case class NotifyAssertFailed(not: Notify[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      not.blame.blame(NotifyFailed(not, error.failure))
  }

  case class LockInvariantOrigin(cls: Class[_]) extends Origin {
    override def preferredName: String = "lock_inv_" + cls.o.preferredName
    override def shortPosition: String = cls.intrinsicLockInvariant.o.shortPosition
    override def context: String = cls.intrinsicLockInvariant.o.context
    override def inlineContext: String = cls.intrinsicLockInvariant.o.inlineContext
  }

  case class HeldTokenOrigin(cls: Class[_]) extends Origin {
    override def preferredName: String = "lock_held_" + cls.o.preferredName
    override def shortPosition: String = cls.intrinsicLockInvariant.o.shortPosition
    override def context: String = cls.intrinsicLockInvariant.o.context
    override def inlineContext: String = cls.intrinsicLockInvariant.o.inlineContext
  }

  case class CommittedOrigin(cls: Class[_]) extends Origin {
    override def preferredName: String = "lock_committed_" + cls.o.preferredName
    override def shortPosition: String = cls.intrinsicLockInvariant.o.shortPosition
    override def context: String = cls.intrinsicLockInvariant.o.context
    override def inlineContext: String = cls.intrinsicLockInvariant.o.inlineContext
  }

  case class LockLockObjectNull(lock: Lock[_]) extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit = error match {
      case InstanceNull(_) => lock.blame.blame(LockObjectNull(lock))
      case failure: InvocationFailure => PanicBlame("elp").blame(failure)
    }
  }

  case class CommittedLockObjectNull(commit: Committed[_]) extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit = error match {
      case InstanceNull(_) => commit.blame.blame(LockObjectNull(commit))
      case failure: InvocationFailure => PanicBlame("elp").blame(failure)
    }
  }

  case class NotCommittedAssertFailed(lock: Lock[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      lock.blame.blame(LockNotCommitted(lock))
  }
}

case class EncodeIntrinsicLock[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeIntrinsicLock._

  val invariant: SuccessionMap[Class[Pre], InstancePredicate[Post]] = SuccessionMap()
  val held: SuccessionMap[Class[Pre], InstancePredicate[Post]] = SuccessionMap()
  val committed: SuccessionMap[Class[Pre], InstanceFunction[Post]] = SuccessionMap()
  val needsHeld: mutable.Set[Class[Pre]] = mutable.Set()
  val needsCommitted: mutable.Set[Class[Pre]] = mutable.Set()

  def getClass(obj: Expr[Pre]): Class[Pre] = obj.t match {
    case TClass(Ref(cls)) => cls
    case _ => throw UnreachableAfterTypeCheck("This argument is not a class type.", obj)
  }

  def needHeld(e: Expr[Pre]): Unit = {
    needsHeld += getClass(e)
    needsCommitted += getClass(e)
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.transSubnodes.foreach {
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

  def needsInvariant(cls: Class[Pre]): Boolean =
    cls.intrinsicLockInvariant != tt[Pre]

  def needsInvariant(e: Expr[Pre]): Boolean =
    needsInvariant(getClass(e))

  def getInvariant(obj: Expr[Pre])(implicit o: Origin): InstancePredicateApply[Post] =
    InstancePredicateApply(dispatch(obj), invariant.ref(getClass(obj)), Nil, WritePerm())

  def getHeld(obj: Expr[Pre])(implicit o: Origin): InstancePredicateApply[Post] =
    InstancePredicateApply(dispatch(obj), held.ref(getClass(obj)), Nil, WritePerm())

  def getCommitted(obj: Expr[Pre])(blame: Blame[InstanceInvocationFailure])(implicit o: Origin): InstanceFunctionInvocation[Post] =
    InstanceFunctionInvocation[Post](dispatch(obj), committed.ref(getClass(obj)), Nil, Nil, Nil, Nil)(blame)

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      globalDeclarations.succeed(cls, cls.rewrite(declarations = classDeclarations.collect {
        if(needsInvariant(cls)) {
          invariant(cls) = classDeclarations.declare(
            new InstancePredicate(Nil, Some(dispatch(cls.intrinsicLockInvariant)))(LockInvariantOrigin(cls)))
        }

        if(needsHeld.contains(cls)) {
          held(cls) = classDeclarations.declare(new InstancePredicate(Nil, None)(HeldTokenOrigin(cls)))
        }

        if(needsCommitted.contains(cls)) {
          implicit val o: Origin = CommittedOrigin(cls)
          committed(cls) = classDeclarations.declare(new InstanceFunction(TBool(), Nil, Nil, None, contract(PanicBlame("empty contract")), false)(AbstractApplicable))
        }

        cls.declarations.foreach(dispatch)
      }._1, intrinsicLockInvariant = tt))
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case Held(obj) => getHeld(obj)
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
        if(needsInvariant(obj))
          Block(Seq(
            Assert(getCommitted(obj)(LockLockObjectNull(lock)))(NotCommittedAssertFailed(lock)),
            Inhale(getInvariant(obj)),
            Unfold(getInvariant(obj))(PanicBlame("Unfolding a predicate immediately after inhaling it should never fail.")),
            Inhale(getHeld(obj)),
          ))
        else
          Block(Seq(
            Assert(getCommitted(obj)(LockLockObjectNull(lock)))(NotCommittedAssertFailed(lock)),
            Inhale(getHeld(obj)),
          ))

      case unlock @ Unlock(obj) =>
        if(needsInvariant(obj))
          Block(Seq(
            Fold(getInvariant(obj))(UnlockInvariantFoldFailed(unlock)),
            Exhale(getInvariant(obj))(PanicBlame("Exhaling a predicate immediately after folding it should never fail.")),
            Exhale(getHeld(obj))(UnlockHeldExhaleFailed(unlock)),
            Assume(getCommitted(obj)(PanicBlame("Exhaling held predicate should imply != null"))),
          ))
        else
          Block(Seq(
            Exhale(getHeld(obj))(UnlockHeldExhaleFailed(unlock)),
            Assume(getCommitted(obj)(PanicBlame("Exhaling held predicate should imply != null"))),
          ))

      case wait @ Wait(obj) =>
        dispatch(Block(Seq(Unlock(obj)(wait.blame), Lock(obj)(PanicBlame("Lock cannot fail after holding the lock")))))

      case notify @ Notify(obj) =>
        Assert(getHeld(obj))(NotifyAssertFailed(notify))

      case commit @ Commit(obj) =>
        if(needsInvariant(obj))
          Block(Seq(
            Fold(getInvariant(obj))(CommitFailedFoldFailed(commit)),
            Exhale(getInvariant(obj))(PanicBlame("Exhaling a predicate immediately after folding it should never fail.")),
            Assume(getCommitted(obj)(PanicBlame("Exhaling invariant predicate should imply != null"))),
          ))
        else
          Assume(getCommitted(obj)(PanicBlame("?")))

      case other => rewriteDefault(other)
    }
  }
}
