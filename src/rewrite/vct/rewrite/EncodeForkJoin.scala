package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

object EncodeForkJoin extends RewriterBuilder {

  case class RunnableMethodMissingError(expr: Expr[_]) extends UserError {
    override def code: String = "runnableMethodMissing"

    override def text: String = expr.o.messageInContext("This class is missing a run method.")
  }

  override def key: String = "forkJoin"
  override def desc: String = "Encode fork and join statements with the contract of the run method it refers to."

  case class IdleToken(cls: Class[_]) extends Origin {
    override def preferredName: String = cls.o.preferredName + "Idle"
    override def context: String = cls.o.context
    override def inlineContext: String = cls.o.inlineContext
    override def shortPosition: String = cls.o.shortPosition
  }

  case class RunningToken(cls: Class[_]) extends Origin {
    override def preferredName: String = cls.o.preferredName + "Running"
    override def context: String = cls.o.context
    override def inlineContext: String = cls.o.inlineContext
    override def shortPosition: String = cls.o.shortPosition
  }

  case class ForkMethod(cls: Class[_]) extends Origin {
    override def preferredName: String = "fork" + cls.o.preferredName
    override def context: String = cls.o.context
    override def inlineContext: String = cls.o.inlineContext
    override def shortPosition: String = cls.o.shortPosition
  }

  case class JoinMethod(cls: Class[_]) extends Origin {
    override def preferredName: String = "join" + cls.o.preferredName
    override def context: String = cls.o.context
    override def inlineContext: String = cls.o.inlineContext
    override def shortPosition: String = cls.o.shortPosition
  }

  case class ForkInstanceInvocation(fork: Fork[_]) extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit = error match {
      case InstanceNull(_) => fork.blame.blame(ForkNull(fork))
      case PreconditionFailed(Nil, _, _) => throw BlamePathError
      case PreconditionFailed(FailLeft :: _, _, _) => fork.blame.blame(RunnableNotIdle(fork))
      case PreconditionFailed(FailRight :: _, failure, _) => fork.blame.blame(RunnablePreconditionNotEstablished(fork, failure))
      case ContextEverywhereFailedInPre(_, _) => PanicBlame("fork method does not have c_e").blame(error)
    }
  }

  case class JoinInstanceInvocation(join: Join[_]) extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit = error match {
      case InstanceNull(_) => join.blame.blame(JoinNull(join))
      case PreconditionFailed(_, _, _) => join.blame.blame(RunnableNotRunning(join))
      case ContextEverywhereFailedInPre(_, _) => PanicBlame("join method does not have c_e").blame(error)
    }
  }
}

case class EncodeForkJoin[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeForkJoin._

  val idleToken: SuccessionMap[Class[Pre], InstancePredicate[Post]] = SuccessionMap()
  val runningToken: SuccessionMap[Class[Pre], InstancePredicate[Post]] = SuccessionMap()

  val forkMethod: SuccessionMap[Class[Pre], InstanceMethod[Post]] = SuccessionMap()
  val joinMethod: SuccessionMap[Class[Pre], InstanceMethod[Post]] = SuccessionMap()

  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = stat match {
    case fork @ Fork(obj) =>
      implicit val o: Origin = stat.o
      val cls = getRunnableClass(obj)
      InvokeMethod[Post](dispatch(obj), forkMethod.ref(cls), Nil, Nil, Nil, Nil, Nil)(ForkInstanceInvocation(fork))
    case join @ Join(obj) =>
      implicit val o: Origin = stat.o
      val cls = getRunnableClass(obj)
      InvokeMethod[Post](dispatch(obj), joinMethod.ref(cls), Nil, Nil, Nil, Nil, Nil)(JoinInstanceInvocation(join))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case vct.col.ast.IdleToken(obj) =>
      implicit val o: Origin = e.o
      val cls = getRunnableClass(obj)
      InstancePredicateApply[Post](dispatch(obj), idleToken.ref(cls), Nil, WritePerm())
    case vct.col.ast.JoinToken(obj) =>
      implicit val o: Origin = e.o
      val cls = getRunnableClass(obj)
      InstancePredicateApply[Post](dispatch(obj), runningToken.ref(cls), Nil, WritePerm())

    case NewObject(Ref(cls)) =>
      implicit val o: Origin = e.o
      cls.declarations.collectFirst {
        case run: RunMethod[Pre] => run
      } match {
        case Some(_) =>
          val obj = new Variable[Post](TClass(succ(cls)))
          ScopedExpr(Seq(obj), With(Block(Seq(
            assignLocal(obj.get, NewObject(succ(cls))),
            Inhale(InstancePredicateApply[Post](obj.get, idleToken.ref(cls), Nil, WritePerm()))
          )), obj.get))
        case None => NewObject(succ(cls))
      }

    case other => rewriteDefault(other)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] => currentClass.having(cls) { rewriteDefault(cls) }
    case m: RunMethod[Pre] =>
      implicit val o: Origin = m.o
      val cls = currentClass.top
      idleToken(cls) = classDeclarations.declare(new InstancePredicate(Nil, None)(IdleToken(cls)))
      runningToken(cls) = classDeclarations.declare(new InstancePredicate(Nil, None)(RunningToken(cls)))
      forkMethod(cls) = classDeclarations.declare(new InstanceMethod(TVoid(), Nil, Nil, Nil, None, contract(
        blame = UnsafeDontCare.Satisfiability("satisfiability is checked by verifying the run method"),
        requires = SplitAccountedPredicate(
          UnitAccountedPredicate(InstancePredicateApply[Post](ThisObject(succ(cls)), idleToken.ref(cls), Nil, WritePerm())),
          dispatch(m.contract.requires),
        ),
        ensures = UnitAccountedPredicate(InstancePredicateApply[Post](ThisObject(succ(cls)), runningToken.ref(cls), Nil, WritePerm()))
      ))(AbstractApplicable)(ForkMethod(cls)))
      joinMethod(cls) = classDeclarations.declare(new InstanceMethod(TVoid(), Nil, Nil, Nil, None, contract(
        blame = PanicBlame("one predicate resource is obviously satisfiable"),
        requires = UnitAccountedPredicate(InstancePredicateApply[Post](ThisObject(succ(cls)), runningToken.ref(cls), Nil, WritePerm())),
        ensures = SplitAccountedPredicate(
          UnitAccountedPredicate(InstancePredicateApply[Post](ThisObject(succ(cls)), idleToken.ref(cls), Nil, WritePerm())),
          dispatch(m.contract.ensures),
        ),
      ))(AbstractApplicable)(JoinMethod(cls)))

      classDeclarations.declare(new InstanceMethod[Post](TVoid(), Nil, Nil, Nil, m.body.map(dispatch), dispatch(m.contract))(m.blame))
    case other => rewriteDefault(other)
  }

  /**
    * Performs a check on whether a class has a runnable method to perform an expression that requires it.
    *   Throws an exception if there is no runnable method.
    * @param expr - the expression that requires the class to have a runnable method
    * @return The class of a given object
    * @throws \RunnableMethodMissingError if the class is detected not to have a Runnable method.
    */
  def getRunnableClass(expr: Expr[Pre]): Class[Pre] = {
    val classType = expr.t.asInstanceOf[TClass[Pre]]
    val check = classType.cls.decl.declarations.collectFirst {
      case _: RunMethod[Pre] => () //return unit
    }.nonEmpty
    if (!check) {
      throw RunnableMethodMissingError(expr)
    }
    classType.cls.decl
  }

}
