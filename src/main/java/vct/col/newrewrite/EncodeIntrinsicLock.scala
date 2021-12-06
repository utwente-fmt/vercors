package vct.col.newrewrite

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.check.UnreachableAfterTypeCheck
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

case object EncodeIntrinsicLock {
  case class UnlockInvariantExhaleFailed(unlock: Unlock) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      unlock.blame.blame(UnlockInvariantFailed(unlock, error.failure))
  }

  case class NotifyAssertFailed(not: Notify) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      not.blame.blame(NotifyFailed(not, error.failure))
  }
}

case class EncodeIntrinsicLock() extends Rewriter {
  import EncodeIntrinsicLock._

  val invariant: SuccessionMap[Class, InstancePredicate] = SuccessionMap()
  val held: SuccessionMap[Class, InstancePredicate] = SuccessionMap()

  def getClass(obj: Expr): Class = obj.t match {
    case TClass(Ref(cls)) => cls
    case _ => throw UnreachableAfterTypeCheck("This argument is not a class type.", obj)
  }

  def getInvariant(obj: Expr)(implicit o: Origin): InstancePredicateApply =
    InstancePredicateApply(dispatch(obj), invariant.ref(getClass(obj)), Nil)

  def getHeld(obj: Expr)(implicit o: Origin): InstancePredicateApply =
    InstancePredicateApply(dispatch(obj), held.ref(getClass(obj)), Nil)

  override def dispatch(decl: Declaration): Unit = decl match {
    case cls: Class if cls.intrinsicLockInvariant != tt =>
      cls.rewrite(declarations = collectInScope(classScopes) {
        invariant(cls) = new InstancePredicate(Nil, Some(dispatch(cls.intrinsicLockInvariant)))(cls.intrinsicLockInvariant.o)
        held(cls) = new InstancePredicate(Nil, None)(cls.intrinsicLockInvariant.o)
        invariant(cls).declareDefault(this)
        held(cls).declareDefault(this)
        cls.declarations.foreach(dispatch)
      })
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = {
    implicit val o: Origin = e.o
    e match {
      case Held(obj) => getHeld(obj)
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(stat: Statement): Statement = {
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
        Unfold(getInvariant(obj)),
        Inhale(getHeld(obj)),
      ))

      case unlock @ Unlock(obj) => Block(Seq(
        Fold(getInvariant(obj)),
        Exhale(getInvariant(obj))(PanicBlame("Exhaling a predicate immediately after folding it should never fail.")),
        Exhale(getHeld(obj))(UnlockInvariantExhaleFailed(unlock)),
      ))

      case wait @ Wait(obj) =>
        dispatch(Block(Seq(Unlock(obj)(wait.blame), Lock(obj))))

      case notify @ Notify(obj) =>
        Assert(getHeld(obj))(NotifyAssertFailed(notify))

      case commit @ Commit(obj) =>
        Block(Seq(
          Fold(getInvariant(obj)),
          Exhale(getInvariant(obj))(PanicBlame("Exhaling a predicate immediately after folding it should never fail.")),
        ))

      case other => rewriteDefault(other)
    }
  }
}
