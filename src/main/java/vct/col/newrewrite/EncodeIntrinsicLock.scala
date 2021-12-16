package vct.col.newrewrite

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.check.UnreachableAfterTypeCheck
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

case object EncodeIntrinsicLock extends RewriterBuilder {
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
    override def messageInContext(message: String): String =
      cls.intrinsicLockInvariant.o.messageInContext(message)
  }

  case class HeldTokenOrigin(cls: Class[_]) extends Origin {
    override def preferredName: String = "lock_held_" + cls.o.preferredName
    override def messageInContext(message: String): String =
      cls.intrinsicLockInvariant.o.messageInContext(message)
  }
}

case class EncodeIntrinsicLock[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeIntrinsicLock._

  val invariant: SuccessionMap[Class[Pre], InstancePredicate[Post]] = SuccessionMap()
  val held: SuccessionMap[Class[Pre], InstancePredicate[Post]] = SuccessionMap()

  def getClass(obj: Expr[Pre]): Class[Pre] = obj.t match {
    case TClass(Ref(cls)) => cls
    case _ => throw UnreachableAfterTypeCheck("This argument is not a class type.", obj)
  }

  def getInvariant(obj: Expr[Pre])(implicit o: Origin): InstancePredicateApply[Post] =
    InstancePredicateApply(dispatch(obj), invariant.ref(getClass(obj)), Nil, WritePerm())

  def getHeld(obj: Expr[Pre])(implicit o: Origin): InstancePredicateApply[Post] =
    InstancePredicateApply(dispatch(obj), held.ref(getClass(obj)), Nil, WritePerm())

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      cls.rewrite(declarations = collectInScope(classScopes) {
        invariant(cls) = new InstancePredicate(Nil, Some(dispatch(cls.intrinsicLockInvariant)))(LockInvariantOrigin(cls))
        held(cls) = new InstancePredicate(Nil, None)(HeldTokenOrigin(cls))
        invariant(cls).declareDefault(this)
        held(cls).declareDefault(this)
        cls.declarations.foreach(dispatch)
      }, intrinsicLockInvariant = tt).succeedDefault(this, decl)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case Held(obj) => getHeld(obj)
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case sync @ Synchronized(obj, body) =>
        TryCatchFinally(
          body = Block(Seq(dispatch(Lock(obj)), dispatch(body))),
          catches = Nil,
          after = dispatch(Unlock(obj)(sync.blame)),
        )

      case Lock(obj) => Block(Seq(
        Inhale(getInvariant(obj)),
        Unfold(getInvariant(obj))(PanicBlame("Unfolding a predicate immediately after inhaling it should never fail.")),
        Inhale(getHeld(obj)),
      ))

      case unlock @ Unlock(obj) => Block(Seq(
        Fold(getInvariant(obj))(UnlockInvariantFoldFailed(unlock)),
        Exhale(getInvariant(obj))(PanicBlame("Exhaling a predicate immediately after folding it should never fail.")),
        Exhale(getHeld(obj))(UnlockHeldExhaleFailed(unlock)),
      ))

      case wait @ Wait(obj) =>
        dispatch(Block(Seq(Unlock(obj)(wait.blame), Lock(obj))))

      case notify @ Notify(obj) =>
        Assert(getHeld(obj))(NotifyAssertFailed(notify))

      case commit @ Commit(obj) =>
        Block(Seq(
          Fold(getInvariant(obj))(CommitFailedFoldFailed(commit)),
          Exhale(getInvariant(obj))(PanicBlame("Exhaling a predicate immediately after folding it should never fail.")),
        ))

      case other => rewriteDefault(other)
    }
  }
}
